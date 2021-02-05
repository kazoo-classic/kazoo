%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bandwidth2_cnam).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").
-include("knm_bandwidth2.hrl").

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
    _ = remove_cnam(Number),
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
        'undefined' when Feature /= 'undefined' ->
            {'ok', Number1} = remove_cnam(Number),
            knm_providers:deactivate_feature(Number1, ?FEATURE_CNAM_OUTBOUND);
        'undefined' ->
            knm_providers:deactivate_feature(Number, ?FEATURE_CNAM_OUTBOUND);
        CurrentCNAM -> Number;
        NewCNAM when IsDryRun ->
            lager:debug("dry run: cnam display name changed to ~s", [NewCNAM]),
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            knm_providers:activate_feature(Number, {?FEATURE_CNAM_OUTBOUND, FeatureData});    
        NewCNAM ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            case assign_cnam(Number, NewCNAM) of
                {'error', _}=E -> E;
                {'ok', Number1} -> 
                    Number2 = knm_providers:activate_feature(Number1, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
                    _ = publish_cnam_update(Number2),
                    Number2
            end
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

%%<LidbOrder>
%%  <CustomerOrderId>[OrderNumber]</CustomerOrderId>
%%  <LidbTnGroups>
%%      <LidbTnGroup>
%%          <TelephoneNumbers>
%%              <TelephoneNumber>6502572250</TelephoneNumber>
%%          </TelephoneNumbers>
%%          <SubscriberInformation>[CNAM]</SubscriberInformation>
%%          <UseType>BUSINESS</UseType>
%%          <Visibility>PUBLIC</Visibility>
%%      </LidbTnGroup>
%%   </LidbTnGroups>
%%</LidbOrder>
-spec assign_cnam(knm_number:knm_number(), kz_term:ne_binary()) ->
          {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
assign_cnam(Number, CNam) ->
    PhoneNumber = knm_number:phone_number(Number),
    Num = to_bandwidth2(knm_phone_number:number(PhoneNumber)),
    AuthBy = knm_phone_number:auth_by(PhoneNumber),
    Props = [{'CustomerOrderId', [kz_term:to_list(AuthBy)]}
            ,{'LidbTnGroups',
              [{'LidbTnGroup', 
                    [
                        {'TelephoneNumbers', [{'TelephoneNumber', [binary_to_list(Num)]}]},
                        {'SubscriberInformation', [binary_to_list(CNam)] },
                        {'UseType', ["BUSINESS"]},
                        {'Visibility', ["PUBLIC"]}
                    ]
              }]
             }
            ],
    Body = xmerl:export_simple([{'LidbOrder', Props}], 'xmerl_xml'),

    case api_post(url(["lidbs"], options(Number)), Body, options(Number)) of
        {'error', Reason} ->
            Error = <<"Unable to assign CNAM: ", (kz_term:to_binary(Reason))/binary>>,
            knm_errors:by_carrier(?MODULE, Error, Num);
        {'ok', Xml} ->
            [Response] = xmerl_xpath:string("LidbOrder", Xml),
            OrderId = kz_xml:get_value("LidbOrder/OrderId/text()", Xml),
            OrderStatus = kz_xml:get_value("LidbOrder/ProcessingStatus/text()", Xml),
            check_order(OrderId, OrderStatus, Response, PhoneNumber, Number, options(Number))
    end.

-spec remove_cnam(knm_number:knm_number()) ->
          {'ok', knm_number:knm_number()} |
          {'error', kz_term:ne_binary()}.
remove_cnam(Number) ->
    {'ok', Number}.
%    PhoneNumber = knm_number:phone_number(Number),
%    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
%    Tns = [kz_json:from_list([
%                              {<<"features">>, features(Number)}
%                              ,{<<"caller_id">>, 'null'}
%                              ,{<<"did">>, Num}
%                              ,{<<"account_location_id">>, location_id(Number)}
%                             ])],
%    Setters = [{fun(J, V) -> kz_json:set_value(<<"order">>, J, V) end, kz_json:new()},
%               {fun(J, V) -> kz_json:set_value(<<"tns">>, V, J) end, Tns}
%              ],
%    JObj = kz_json:set_values(Setters, kz_json:new()),
%    case knm_thinq_util:api_post(url_feature_order(options(Number)), JObj, options(Number)) of
%        {'ok', Results} -> 
%            OrderId = kz_json:get_value([<<"order">>,<<"id">>], Results),
%            OrderStatus = kz_json:get_value([<<"order">>,<<"status">>], Results),
%            'ok' = complete_feature_order(OrderId, OrderStatus, Results, Number),
%            {'ok', set_cnam_id(Number, 'null')};
%        {'error', Reason} -> 
%            Error = <<"Unable to remove CNAM: ", (kz_term:to_binary(Reason))/binary>>,
%            knm_errors:invalid(Num, Error)
%    end.

-spec url([nonempty_string()], knm_search:options()) -> nonempty_string().
url(RelativePath, Options) ->
    lists:flatten(
      [io_lib:format("~s/accounts/~s/", [?BW2_BASE_URL, ?ACCOUNT_ID(Options)])
       | RelativePath
      ]).

-spec auth(knm_search:options()) -> {'basic_auth', {kz_term:ne_binary(), kz_term:ne_binary()}}.
auth([]) ->
    {'basic_auth', {?BW2_API_USERNAME, ?BW2_API_PASSWORD}};
auth(Options) ->
    Account_id = knm_carriers:account_id(Options),
    Reseller_id = knm_carriers:reseller_id(Options),
    {'basic_auth', {?BW2_API_USERNAME(Account_id, Reseller_id), ?BW2_API_PASSWORD(Account_id, Reseller_id)}}.

-spec api_post(nonempty_string(), binary(), knm_search:options()) -> api_res().
api_post(Url, Body, Options) ->
    UnicodeBody = unicode:characters_to_binary(Body),
    Headers = [{"Accept", "*/*"}
              ,{"User-Agent", ?KNM_USER_AGENT}
              ,{"X-BWC-IN-Control-Processing-Type", "process"}
              ,{"Content-Type", "application/xml"}
              ],
    HTTPOptions = [auth(Options)
                  ,{'ssl', [{'verify', 'verify_none'}]}
                  ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'body_format', 'string'}
                  ],
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n~p~n~p~p~n", ['post', Url, Headers, HTTPOptions, Body, UnicodeBody]),
    Response = kz_http:post(Url, Headers, UnicodeBody, HTTPOptions),
    handle_response(Response).

-type api_res() :: {'ok', kz_types:xml_el()} | {'error', atom()}.

%%------------------------------------------------------------------------------
%% @doc Make a REST request to Bandwidth.com Numbers API to perform the
%% given verb (purchase, search, provision, etc).
%% @end
%%------------------------------------------------------------------------------
-spec handle_response(kz_http:ret()) -> api_res().
handle_response({Result, Code, Props, Response})
  when is_binary(Response) ->
    handle_response({Result, Code, Props, kz_term:to_list(Response)});
handle_response({'ok', 401, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n401~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 401 (unauthenticated)"),
    {'error', 'authentication'};

handle_response({'ok', 403, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n403~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 403 (unauthorized)"),
    {'error', 'authorization'};

handle_response({'ok', 404, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n404~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 404 (not found)"),
    {'error', 'not_found'};

handle_response({'ok', 500, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n500~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 500 (server error)"),
    {'error', 'server_error'};

handle_response({'ok', 503, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n503~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 503"),
    {'error', 'server_error'};

handle_response({'ok', Code, Headers, "<?xml"++_=Response}) ->
    ?DEBUG_APPEND("Response:~nCode : ~p~nBody : ~s~nHeaders : ~p~n", [Code, Response, Headers]),
    lager:debug("received response from bandwidth.com"),
    try
        {Xml, _} = xmerl_scan:string(Response),
        verify_response(Xml)
    catch
        _:R ->
            lager:debug("failed to decode xml: ~p", [R]),
            {'error', 'empty_response'}
    end;

handle_response({'ok', Code, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~s~n", [Code, _Response]),
    lager:debug("bandwidth.com empty response: ~p", [Code]),
    {'error', 'empty_response'};

handle_response({'error', _}=E) ->
    lager:debug("bandwidth.com request error: ~p", [E]),
    E.

-spec verify_response(kz_types:xml_el()) -> {'ok', kz_types:xml_el()} |
                                            {'error', any()}.
verify_response(Xml) ->
    TNs = "count(//LidbTnGroups/LidbTnGroup/TelephoneNumbers/TelephoneNumber)",
    case validate_xpath_value(xmerl_xpath:string(TNs, Xml))
        orelse validate_xpath_value(kz_xml:get_value("//ProcessingStatus/text()", Xml))
    of
        'true' ->
            lager:debug("request was successful"),
            {'ok', Xml};
        'false' ->
            ErrorPath = "//ErrorList/Error/Description/text()",
            Reason = case kz_xml:get_value(ErrorPath, Xml) of
                         'undefined' -> <<"undefined">>;
                         R -> R
                     end,
            lager:debug("request failed: ~s", [Reason]),
            {'error', Reason}
    end.

-spec validate_xpath_value(kz_term:api_binary() | {atom(), atom(), non_neg_integer()}) -> boolean().
validate_xpath_value('undefined') -> 'false';
validate_xpath_value(<<>>) -> 'false';
validate_xpath_value({'xmlObj', 'number', Num}) -> Num > 0;
validate_xpath_value(_) -> 'true'.


-spec check_order(kz_term:api_binary(), kz_term:ne_binary(), kz_types:xml_el(), knm_phone_number:knm_phone_number(), knm_number:knm_number(), list()) -> knm_number:knm_number().
check_order(_OrderId, OrderStatus, Response, PhoneNumber, Number, _Options) 
            when OrderStatus =:= <<"RECEIVED">>
                 orelse OrderStatus =:= <<"PROCESSING">>
                 orelse OrderStatus =:= <<"COMPLETE">> ->
    OrderData = order_response_to_json(Response),
    PN = knm_phone_number:update_carrier_data(PhoneNumber, OrderData),
    {'ok', knm_number:set_phone_number(Number, PN)};
check_order(_OrderId, <<"FAILED">>, Response, PhoneNumber, _Number, _Options) ->
    Reason = kz_xml:get_value("//Error/Description/text()", Response),
    Error = <<"Unable to set CNAM: ", (kz_term:to_binary(Reason))/binary>>,
    Num = to_bandwidth2(knm_phone_number:number(PhoneNumber)),
    knm_errors:by_carrier(?MODULE, Error, Num);
check_order(_OrderId, OrderStatus, _Response, PhoneNumber, _Number, _Options) ->
    Error = <<"Unable to set CNAM: ", (kz_term:to_binary(OrderStatus))/binary>>,
    Num = to_bandwidth2(knm_phone_number:number(PhoneNumber)),
    knm_errors:by_carrier(?MODULE, Error, Num).

-spec to_bandwidth2(kz_term:ne_binary()) -> kz_term:ne_binary().
to_bandwidth2(<<"+1", Number/binary>>) -> Number;
to_bandwidth2(Number) -> Number.

-spec from_bandwidth2(kz_term:ne_binary()) -> kz_term:ne_binary().
from_bandwidth2(<<"+", _/binary>> = Number) -> Number;
from_bandwidth2(Number) -> <<"+1", Number/binary>>.

options(Number) ->
    {'ok', AccountId, ResellerId} = knm_thinq_util:get_account_and_reseller_id(Number),
    [{account_id, AccountId}
     ,{reseller_id, ResellerId}
     ].

-spec order_response_to_json(kz_types:xml_els() | kz_types:xml_el()) -> kz_json:object().
order_response_to_json([]) ->
    kz_json:new();
order_response_to_json([Xml]) ->
    order_response_to_json(Xml);
order_response_to_json(Xml) ->
    Num = from_bandwidth2(kz_xml:get_value("LidbTnGroups/LidbTnGroup/TelephoneNumbers/TelephoneNumber/text()", Xml)),
    kz_json:from_list(
      props:filter_empty(
        [{<<"customer_order_id">>, kz_xml:get_value(?CUSTOMER_ORDER_ID_XPATH, Xml)}
        ,{<<"order_id">>,kz_xml:get_value("OrderId/text()", Xml)}
        ,{<<"number">>, Num}
        ]
       )
     ).