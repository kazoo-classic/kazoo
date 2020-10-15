%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_thinq_cnam).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").
-include("knm_thinq.hrl").

-define(CNAM_ID, <<"cnam_id">>).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

-spec save(knm_number:knm_number(), kz_term:ne_binary()) -> knm_number:knm_number().
save(Number, ?NUMBER_STATE_RESERVED) ->
    handle(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    handle(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    handle(Number);
save(Number, _State) ->
    Number.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    _ = remove_outbound_cnam(Number),
    knm_providers:deactivate_features(Number
                                     ,[?FEATURE_CNAM_INBOUND
                                      ,?FEATURE_CNAM_OUTBOUND
                                      ,?FEATURE_CNAM
                                      ]
                                     ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(knm_number:knm_number()) -> knm_number:knm_number().
handle(Number) ->
    support_depreciated_cnam(
      handle_inbound_cnam(
        handle_outbound_cnam(
          Number
         )
       )
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_outbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
handle_outbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    CurrentCNAM = kz_json:get_ne_value(?CNAM_DISPLAY_NAME, Feature),
    IsDryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    case kz_json:get_ne_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], Doc) of
        'undefined' ->
            knm_providers:deactivate_feature(Number, ?FEATURE_CNAM_OUTBOUND);
        CurrentCNAM -> Number;
        NewCNAM when IsDryRun ->
            lager:debug("dry run: cnam display name changed to ~s", [NewCNAM]),
            knm_providers:activate_feature(Number, {?FEATURE_CNAM_OUTBOUND, NewCNAM});    
        NewCNAM ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            Number1 = knm_providers:activate_feature(Number, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
            case create_outbound_cnam(Number1, NewCNAM) of
                {'error', _}=E -> E;
                {'ok', CNamId} -> assign_outbound_cnam(Number1, CNamId)
            end,
            _ = publish_cnam_update(Number1),
            Number1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_inbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
handle_inbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PhoneNumber),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_INBOUND),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        false ->
            knm_providers:deactivate_features(Number, [?FEATURE_CNAM_INBOUND
                                                      ,?CNAM_INBOUND_LOOKUP
                                                      ]);
        'true' ->
            case kz_json:is_true(?CNAM_INBOUND_LOOKUP, Feature) of
                'true' -> Number;
                'false' ->
                    FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
                    knm_providers:activate_feature(Number, {?FEATURE_CNAM_INBOUND, FeatureData})
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec support_depreciated_cnam(knm_number:knm_number()) -> knm_number:knm_number().
support_depreciated_cnam(Number) ->
    knm_providers:deactivate_feature(Number, ?FEATURE_CNAM).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec publish_cnam_update(knm_number:knm_number()) -> 'ok'.
publish_cnam_update(Number) ->
    DryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    publish_cnam_update(Number, DryRun).

-spec publish_cnam_update(knm_number:knm_number(), boolean()) -> 'ok'.
publish_cnam_update(_Number, 'true') -> 'ok';
publish_cnam_update(Number, 'false') ->
    PhoneNumber = knm_number:phone_number(Number),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PhoneNumber)}
             ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_util:pretty_print(knm_phone_number:number(PhoneNumber))}
             ,{<<"Acquired-For">>, knm_phone_number:auth_by(PhoneNumber)}
             ,{<<"Cnam">>, case Feature of 'undefined' -> kz_json:new(); _ -> Feature end}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_cnam_request/1).

-spec create_outbound_cnam(knm_number:knm_number(),  kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', kz_term:ne_binary() | any()}.
create_outbound_cnam(Number, CNAM) ->
    AccountCallerId = kz_json:from_list([{<<"caller_id_type">>, <<"business">>}
                                        ,{<<"name">>, CNAM}]),
    case knm_thinq_util:api_post(url_outbound_cnam(), 
                                 kz_json:set_value(<<"AccountCallerId">>,AccountCallerId, kz_json:new())) of
        {ok, Rep} ->
            CNamId = kz_json:get_ne_binary_value(<<"id">>, Rep),
            lager:debug("created CNAM ~s", [kz_json:encode(Rep)]),
            {'ok', CNamId};
        {'error', Reason} -> 
            Error = <<"Unable to create CNAM : ", (kz_term:to_binary(Reason))/binary>>,
            knm_errors:by_carrier(?MODULE, Error, Number)
        end. 

-spec assign_outbound_cnam(knm_number:knm_number(), kz_term:ne_binary()) ->
          {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
assign_outbound_cnam(Number, CNamId) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
    Tns = [kz_json:from_list([
                              {<<"features">>, cnam_feature('true', Number)},
                              {<<"caller_id">>, CNamId},
                              {<<"did">>, Num}
                             ])],
    Setters = [{fun(J, V) -> kz_json:set_value(<<"order">>, J, V) end, kz_json:new()},
               {fun(J, V) -> kz_json:set_value(<<"tns">>, V, J) end, Tns}
              ],
    JObj = kz_json:set_values(Setters, kz_json:new()),
    case knm_thinq_util:api_post(url_feature_order(), JObj) of
        {'ok', Results} -> 
            OrderId = kz_json:get_value([<<"order">>,<<"id">>], Results),
            OrderStatus = kz_json:get_value([<<"order">>,<<"status">>], Results),
            complete_feature_order(CNamId, OrderId, OrderStatus, Results, knm_number:phone_number(Number), Number);
        {'error', Reason} -> 
            Error = <<"Unable to assign CNAM: ", (kz_term:to_binary(Reason))/binary>>,
            knm_errors:by_carrier(?MODULE, Error, Num)
    end.

-spec remove_outbound_cnam(knm_number:knm_number()) ->
          {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
remove_outbound_cnam(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
    Tns = [kz_json:from_list([
                              {<<"features">>, cnam_feature('false', Number)},
                              {<<"did">>, Num}
                             ])],
    Setters = [{fun(J, V) -> kz_json:set_value(<<"order">>, J, V) end, kz_json:new()},
               {fun(J, V) -> kz_json:set_value(<<"tns">>, V, J) end, Tns}
              ],
    JObj = kz_json:set_values(Setters, kz_json:new()),
    case knm_thinq_util:api_post(url_feature_order(), JObj) of
        {'ok', Results} -> 
            OrderId = kz_json:get_value([<<"order">>,<<"id">>], Results),
            OrderStatus = kz_json:get_value([<<"order">>,<<"status">>], Results),
            complete_feature_order('null', OrderId, OrderStatus, Results, knm_number:phone_number(Number), Number);
        {'error', Reason} -> 
            Error = <<"Unable to remove CNAM: ", (kz_term:to_binary(Reason))/binary>>,
            knm_errors:by_carrier(?MODULE, Error, Num)
    end.

-spec cnam_feature(boolean(), knm_number:knm_number()) ->  kz_json:object().
cnam_feature(State, Number) ->
    Features = knm_phone_number:features(knm_number:phone_number(Number)),
    kz_json:from_list([{<<"cnam">>, State},
                       {<<"sms">>, kz_json:is_json_object(?FEATURE_SMS, Features)},
                       {<<"e911">>, kz_json:is_json_object(?FEATURE_E911, Features)}
                      ]).



-spec complete_feature_order(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kz_types:xml_el(), knm_phone_number:knm_phone_number(), knm_number:knm_number()) -> knm_number:knm_number().
complete_feature_order(CNamId, OrderId, <<"created">>, _Response, PhoneNumber, Number) ->
    case knm_thinq_util:api_post(url_complete_order(OrderId), kz_json:new()) of
        {'ok', _OrderData} -> 
            {'ok', set_cnam_id(Number, CNamId)};
        {'error', Reason} -> 
            Error = <<"Unable to complete order: ", (kz_term:to_binary(Reason))/binary>>,
            Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
            knm_errors:by_carrier(?MODULE, Error, Num)
    end;
complete_feature_order(_CNamId, _OrderId, _, _Response, PhoneNumber, _Number) ->
    Reason = <<"FAILED">>,
    Error = <<"Unable to assign CNAM: ", (kz_term:to_binary(Reason))/binary>>,
    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
    knm_errors:by_carrier(?MODULE, Error, Num).

-spec set_cnam_id(knm_number:knm_number(), kz_term:ne_binary() | 'null') -> knm_number:knm_number().
set_cnam_id(Number, CNamId) ->
    PN = knm_number:phone_number(Number),
    Data = kz_json:from_list([{?CNAM_ID, CNamId}]),
    NewPN = knm_phone_number:update_carrier_data(PN, Data),
    knm_number:set_phone_number(Number, NewPN).

%{{host}}/account/{{account_id}}/caller_id/
-spec url_outbound_cnam() -> nonempty_string().
url_outbound_cnam() ->
    lists:flatten(
        io_lib:format("~s/account/~s/caller_id", [?THQ_BASE_URL, ?THQ_ACCOUNT_ID])
    ).

%{{host}}/account/{{account_id}}/origination/did/features/create
-spec url_feature_order() -> nonempty_string().
url_feature_order() ->
    lists:flatten(
        io_lib:format("~s/account/~s/origination/did/features/create", [?THQ_BASE_URL, ?THQ_ACCOUNT_ID])
    ).

%{{host}}/account/{{account_id}}/origination/did/features/complete/495533
-spec url_complete_order(nonempty_string()) -> nonempty_string().
url_complete_order(OrderId) ->
    lists:flatten(
        io_lib:format("~s/account/~s/origination/did/features/complete/~b", [?THQ_BASE_URL, ?THQ_ACCOUNT_ID, OrderId])
    ).
