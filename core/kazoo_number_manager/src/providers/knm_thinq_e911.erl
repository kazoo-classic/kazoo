%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handle e911 provisioning
%%% @author Alan R Evans
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_thinq_e911).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").
-include("knm_thinq.hrl").

-define(ADDRESS_ID, <<"address_id">>).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

-spec save(knm_number:knm_number(), kz_term:ne_binary()) -> knm_number:knm_number().
save(Number, ?NUMBER_STATE_RESERVED) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    maybe_update_e911(Number);
save(Number, _State) ->
    delete(Number).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    case feature(Number) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information"),
            {'ok', NewNumber} = remove_number(Number),
            knm_providers:deactivate_feature(NewNumber, ?FEATURE_E911)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?FEATURE_E911).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec maybe_update_e911(knm_number:knm_number()) -> knm_number:knm_number().
maybe_update_e911(Number) ->
    IsDryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    maybe_update_e911(Number, (IsDryRun
                               orelse ?IS_SANDBOX_PROVISIONING_TRUE
                              )).

-spec maybe_update_e911(knm_number:knm_number(), boolean()) -> knm_number:knm_number().
maybe_update_e911(Number, 'true') ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("dry run: information has been removed, updating upstream"),
            knm_providers:deactivate_feature(Number, ?FEATURE_E911);
        'false' when NotChanged  ->
            Number;
        'false' ->
            lager:debug("dry run: information has been changed: ~s", [kz_json:encode(E911)]),
            knm_providers:activate_feature(Number, {?FEATURE_E911, E911})
    end;

maybe_update_e911(Number, 'false') ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("information has been removed, updating upstream"),
            {'ok', NewNumber} = remove_number(Number),
            knm_providers:deactivate_feature(NewNumber, ?FEATURE_E911);
        'false' when NotChanged  ->
            Number;
        'false' ->
            case update_e911(Number, E911) of
                {'ok', NewNumber} ->
                    lager:debug("information has been changed: ~s", [kz_json:encode(E911)]),
                    knm_providers:activate_feature(NewNumber, {?FEATURE_E911, E911});
                {'error', E} ->
                    lager:error("information update failed: ~p", [E]),
                    knm_errors:unspecified(E, Number)
            end
    end.

-spec update_e911(knm_number:knm_number(), kz_json:object()) ->
          {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
update_e911(Number, AddressJObj) ->
    remove_number_address(Number),
    case create_address(Number, AddressJObj) of
        {'error', _}=E -> E;
        {'ok', AddressId} -> assign_address(Number, AddressId)
    end.

-spec create_address(knm_number:knm_number(), kz_json:object()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', kz_term:ne_binary() | any()}.
create_address(Number, AddressJObj) ->
    Body = kz_json:from_list([{<<"location">>, e911_address(Number, AddressJObj)}
                              ,{<<"account_id">>, ?THQ_ACCOUNT_ID}]),
    case knm_thinq_util:api_post(url_location(), Body) of
        {ok, Rep} ->
            AddressId = kz_json:get_ne_binary_value(<<"id">>, Rep),
            lager:debug("created address ~s", [kz_json:encode(Rep)]),
            {'ok', AddressId};
        {'error', Reason} -> 
            Error = <<"Unable to create e911 address : ", (kz_term:to_binary(Reason))/binary>>,
            knm_errors:by_carrier(?MODULE, Error, Number)
        end.           

-spec assign_address(knm_number:knm_number(), kz_term:ne_binary() | 'null') ->
          {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
assign_address(Number, AddressId) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
    Tns = [kz_json:from_list([
                              {<<"features">>, e911_feature('true', Number)},
                              {<<"account_location_id">>, AddressId},
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
            complete_feature_order(AddressId, OrderId, OrderStatus, Results, knm_number:phone_number(Number), Number);
        {'error', Reason} -> 
            Error = <<"Unable to assign e911 address: ", (kz_term:to_binary(Reason))/binary>>,
            knm_errors:by_carrier(?MODULE, Error, Num)
    end.

-spec remove_address(knm_number:knm_number()) ->
          {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
remove_address(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
    Tns = [kz_json:from_list([
                              {<<"features">>, e911_feature('false', Number)},
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
            Error = <<"Unable to remove e911 address: ", (kz_term:to_binary(Reason))/binary>>,
            knm_errors:by_carrier(?MODULE, Error, Num)
    end.

-spec e911_feature(boolean(), knm_number:knm_number()) ->  kz_json:object().
e911_feature(State, Number) ->
    Features = knm_phone_number:features(knm_number:phone_number(Number)),
    kz_json:from_list([{<<"cnam">>, kz_json:is_json_object(?FEATURE_CNAM_OUTBOUND, Features)},
                       {<<"sms">>, kz_json:is_json_object(?FEATURE_SMS, Features)},
                       {<<"e911">>, State}
                      ]).

-spec set_address_id(knm_number:knm_number(), kz_term:ne_binary() | 'null') -> knm_number:knm_number().
set_address_id(Number, AddressId) ->
    PN = knm_number:phone_number(Number),
    Data = kz_json:from_list([{?ADDRESS_ID, AddressId}]),
    NewPN = knm_phone_number:update_carrier_data(PN, Data),
    knm_number:set_phone_number(Number, NewPN).

-spec remove_number(knm_number:knm_number()) -> {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
remove_number(Number) ->
    CarrierData = knm_phone_number:carrier_data(knm_number:phone_number(Number)),
    case kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData) of
        'undefined' -> {'ok', Number};
        AddressId ->
            lager:debug("removing previously set address: ~p", [AddressId]),
            remove_address(Number)
    end.

-spec remove_number_address(knm_number:knm_number()) -> 'ok'.
remove_number_address(Number) ->
    CarrierData = knm_phone_number:carrier_data(knm_number:phone_number(Number)),
    case kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData) of
        'undefined' -> 'ok';
        AddrId ->
            Path = ["e911_addresses", binary_to_list(AddrId)],
            _ = kz_util:spawn(fun() -> catch knm_thinq_util:req(delete, Path) end),
            'ok'
    end.

-spec e911_address(knm_number:knm_number(), kz_json:object()) -> kz_json:object().
e911_address(Number, JObj) ->
    E911Name = kz_json:get_ne_binary_value(?E911_NAME, JObj),
    CallerName = knm_providers:e911_caller_name(Number, E911Name),
    kz_json:from_list(
      props:filter_empty(
        [{<<"alias">>, CallerName}
        ,{<<"location_type">>, <<"business">>}
        ,{<<"address">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET1, JObj))}
        ,{<<"address2">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET2, JObj))}    
        ,{<<"city">>, cleanse(kz_json:get_ne_binary_value(?E911_CITY, JObj))}
        ,{<<"state">>, cleanse(kz_json:get_ne_binary_value(?E911_STATE, JObj))}
        ,{<<"zip">>, kz_json:get_ne_binary_value(?E911_ZIP, JObj)}
        ,{<<"country">>, <<"US">>}
        ])).

-spec cleanse(kz_term:api_ne_binary()) -> kz_term:api_binary().
cleanse('undefined') -> 'undefined';
cleanse(NEBin) ->
    Upper = kz_term:to_upper_binary(NEBin),
    << <<C>> || <<C>> <= Upper, is_ALnum_or_space(C)>>.

-spec is_ALnum_or_space(char()) -> boolean().
is_ALnum_or_space(C) when $0 =< C, C =< $9 -> 'true';
is_ALnum_or_space(C) when $A =< C, C =< $Z -> 'true';
is_ALnum_or_space(C) -> $\s =:= C.


-spec complete_feature_order(kz_term:api_binary(), kz_term:api_binary(), kz_term:ne_binary(), kz_types:xml_el(), knm_phone_number:knm_phone_number(), knm_number:knm_number()) -> knm_number:knm_number().
complete_feature_order(AddressId, OrderId, <<"created">>, _Response, PhoneNumber, Number) ->
    case knm_thinq_util:api_post(url_complete_order(OrderId), kz_json:new()) of
        {'ok', _OrderData} -> 
            {'ok', set_address_id(Number, AddressId)};
        {'error', Reason} -> 
            Error = <<"Unable to complete order: ", (kz_term:to_binary(Reason))/binary>>,
            Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
            knm_errors:by_carrier(?MODULE, Error, Num)
    end;
complete_feature_order(_AddressId, _OrderId, _, _Response, PhoneNumber, _Number) ->
    Reason = <<"FAILED">>,
    Error = <<"Unable to assign e911 number: ", (kz_term:to_binary(Reason))/binary>>,
    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
    knm_errors:by_carrier(?MODULE, Error, Num).


%{{host}}/account/{{account_id}}/location/
-spec url_location() -> nonempty_string().
url_location() ->
    lists:flatten(
        io_lib:format("~s/account/~s/location", [?THQ_BASE_URL, ?THQ_ACCOUNT_ID])
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
