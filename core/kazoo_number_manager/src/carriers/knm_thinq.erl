%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handle client requests for phone_number documents using thinQ api
%%% @author Alan R Evans
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_thinq).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

%% Maintenance commands
-export([sites/0, peers/1]).

-include("knm.hrl").
-include("knm_thinq.hrl").

-ifdef(TEST).
-export([auth/0]).  %% Only to pass compilation
-endif.


-type search_ret() :: {'ok', knm_number:knm_numbers()} | {'error', any()}.

%%% API


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 3
     }.

%%------------------------------------------------------------------------------
%% @doc Is this carrier handling numbers local to the system?
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', 'not_implemented'}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc Query the Thinq.com system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) -> search_ret().
find_numbers(<<"+", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);

find_numbers(<<"1", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);

find_numbers(<<Prefix:3/binary, _/binary>>=Num, Quantity, Options) when ?IS_US_TOLLFREE(Prefix) ->
    Params = ["maxdids=", quantity_uri_param(Quantity),
              "&rows=", quantity_uri_param(Quantity),
              "&page=", "1"
             ],
    JObj = kz_json:from_list([{<<"search">>, tf_prefix(Num)}]),
    Result = search_tollfree(Num, Params, JObj),
    process_search_response(Result, Options);

find_numbers(<<Prefix:3/binary, _/binary>>=Num, Quantity, Options) when ?IS_US_TOLLFREE_WILDCARD(Prefix) ->
    Params = ["maxdids=", quantity_uri_param(Quantity),
              "&rows=", quantity_uri_param(Quantity),
              "&page=", "1"
             ],
    JObj = kz_json:from_list([{<<"search">>, tf_prefix(Num)}]),
    Result = search_tollfree(Num, Params, JObj),
    process_search_response(Result, Options);

find_numbers(<<NPA:3/binary>>, Quantity, Options) ->
    Params = ["rows=",quantity_uri_param(Quantity),
              "&page=", "1"
             ],
    JObj = kz_json:from_list([{<<"search">>, npanxx(NPA)}]),
    Result = search_regular(NPA, Params, JObj),
    process_search_response(Result, Options);

find_numbers(Search, Quantity, Options) ->
    NpaNxx = kz_binary:truncate_right(Search, 6),
    JObj = kz_json:from_list([{<<"search">>, npanxx(NpaNxx)}]),
    Params = ["rows=", quantity_uri_param(Quantity),
              "&page=", "1"
             ],
    Result = search_regular(NpaNxx, Params, JObj),
    process_search_response(Result, Options).

npanxx(NPA) ->
    kz_json:from_list([{<<"npanxx">>,NPA}]).

tf_prefix(<<P1:1/binary, P2:1/binary, "*">>) ->
    kz_json:from_list([{<<"tf_prefix">>,<<P1/binary,P2/binary,P2/binary>>}]);
tf_prefix(Num) ->
    kz_json:from_list([{<<"tf_prefix">>,Num}]).

process_search_response(JObj, Options) ->
    QID = knm_search:query_id(Options),
    Result = kz_json:get_value(<<"rows">>, JObj),
    Numbers = [{QID, { <<"+1", 
                         (integer_to_binary(kz_json:get_integer_value(<<"id">>, Data)))/binary>>, 
                        ?MODULE, ?NUMBER_STATE_DISCOVERY, Data}}
     || Data <- Result
    ],
    {ok, Numbers}.


%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            PhoneNumber = knm_number:phone_number(Number),
            Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),

            Tns = [kz_json:from_list([{<<"caller_id">>, 'null'},
                                      {<<"account_location_id">>, 'null'},
                                      {<<"sms_routing_profile_id">>, 'null'},
                                      {<<"route_id">>, ?THQ_SITE_ID},
                                      {<<"features">>, features()},
                                      {<<"did">>, Num}
                                     ])],
            Setters = [{fun(J, V) -> kz_json:set_value(<<"blocks">>, V, J) end, []},
                       {fun(J, V) -> kz_json:set_value(<<"order">>, J, V) end, kz_json:new()},
                       {fun(J, V) -> kz_json:set_value(<<"tns">>, V, J) end, Tns}
                      ],
            JObj = kz_json:set_values(Setters, kz_json:new()),

            case knm_thinq_util:api_post(url_purchase(), JObj) of
                {'ok', Results} -> 
                    OrderId = kz_json:get_value(<<"id">>, Results),
                    OrderStatus = kz_json:get_value(<<"status">>, Results),
                    complete_order(OrderId, OrderStatus, Results, PhoneNumber, Number);
                {'error', Reason} -> 
                    Error = <<"Unable to acquire number: ", (kz_term:to_binary(Reason))/binary>>,
                    knm_errors:by_carrier(?MODULE, Error, Num)
            end
    end.

features() ->
    kz_json:from_list([{<<"cnam">>, 'false'},
                       {<<"sms">>, 'true'},
                       {<<"e911">>, 'false'}
                      ]).




-spec complete_order(kz_term:api_binary(), kz_term:ne_binary(), kz_types:xml_el(), knm_phone_number:knm_phone_number(), knm_number:knm_number()) -> knm_number:knm_number().
complete_order(OrderId, <<"created">>, _Response, PhoneNumber, Number) ->
    case knm_thinq_util:api_post(url_complete(OrderId), kz_json:new()) of
        {'ok', OrderData} -> 
            PN = knm_phone_number:update_carrier_data(PhoneNumber, OrderData),
            knm_number:set_phone_number(Number, PN);
        {'error', Reason} -> 
            Error = <<"Unable to complete order: ", (kz_term:to_binary(Reason))/binary>>,
            Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
            knm_errors:by_carrier(?MODULE, Error, Num)
    end;
complete_order(_OrderId, _, _Response, PhoneNumber, _Number) ->
    Reason = <<"FAILED">>,
    Error = <<"Unable to acquire number: ", (kz_term:to_binary(Reason))/binary>>,
    Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
    knm_errors:by_carrier(?MODULE, Error, Num).

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) -> knm_number:knm_number().
disconnect_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            PhoneNumber = knm_number:phone_number(Number),
            Num = knm_thinq_util:to_thinq(knm_phone_number:number(PhoneNumber)),
            IsDryRun = knm_phone_number:dry_run(PhoneNumber),

            JObj = kz_json:set_value(<<"dids">>, [Num], kz_json:new()),

            case not IsDryRun andalso 
                    knm_thinq_util:api_post(url_disconnect(), JObj) of
                'false' -> Number;
                {'ok', _Results} -> 
                    Number;
                {'error', Reason} -> 
                    Error = <<"Unable to disconnect number: ", (kz_term:to_binary(Reason))/binary>>,
                    knm_errors:by_carrier(?MODULE, Error, Num)
            end
    end.

-spec sites() -> 'ok'.
sites() ->
    {'ok', Xml} = knm_thinq_util:api_get(url_search(["sites"])),
    io:format("listing all sites for account ~p~n", [?THQ_ACCOUNT_ID]),
    Sites = xmerl_xpath:string("Sites/Site", Xml),
    lists:foreach(fun process_site/1, Sites),
    io:format("done.~n").

-spec process_site(kz_types:xml_el()) -> 'ok'.
process_site(Site) ->
    Id   = kz_xml:get_value("Id/text()", Site),
    Name = kz_xml:get_value("Name/text()", Site),
    io:format("Id: ~p Name: ~p~n", [Id, Name]).

-spec peers(binary()) -> 'ok'.
peers(SiteId) ->
    {'ok', Xml} = knm_thinq_util:api_get(url_search(["sippeers"])),
    io:format("listing all peers for account ~p, site ~p~n", [?THQ_ACCOUNT_ID, SiteId]),
    Peers = xmerl_xpath:string("SipPeers/SipPeer", Xml),
    lists:foreach(fun process_peer/1, Peers),
    io:format("done.~n").

-spec process_peer(kz_types:xml_el()) -> 'ok'.
process_peer(Peer) ->
    Id   = kz_xml:get_value("PeerId/text()", Peer),
    Name = kz_xml:get_value("PeerName/text()", Peer),
    io:format("Id: ~p Name: ~p~n", [Id, Name]).

%%% Internals
%{{host}}/origination/did/search/individual/{{account_id}}
%{{host}}/origination/did/search/tollfree/{{account_id}}
-spec url_search([nonempty_string()]) -> nonempty_string().
url_search(RelativePath) ->
    lists:flatten(
        io_lib:format("~s~s~s", [?THQ_BASE_URL, RelativePath, ?THQ_ACCOUNT_ID])
    ).

%{{host}}/account/{{account_id}}/origination/order/create
-spec url_purchase() -> nonempty_string().
url_purchase() ->
    lists:flatten(
        io_lib:format("~s/account/~s/origination/order/create", [?THQ_BASE_URL, ?THQ_ACCOUNT_ID])
    ).

%{{host}}/account/{{account_id}}/origination/order/complete/{{order_id}}
-spec url_complete(nonempty_string()) -> nonempty_string().
url_complete(OrderId) ->
    lists:flatten(
        io_lib:format("~s/account/~s/origination/order/complete/~b", [?THQ_BASE_URL, ?THQ_ACCOUNT_ID, OrderId])
    ).

%{{host}}/account/{{account_id}}/origination/disconnect
-spec url_disconnect() -> nonempty_string().
url_disconnect() ->
    lists:flatten(
        io_lib:format("~s/account/~s/origination/disconnect", [?THQ_BASE_URL, ?THQ_ACCOUNT_ID])
    ).



-spec search_regular(nonempty_string(), [nonempty_string()], kz_json:object()) -> kz_json:object().
search_regular(Num, Params, JObj) ->
    case knm_thinq_util:api_post(lists:flatten(
                    [url_search("/origination/did/search/individual/")
                     ,"?" 
                     ,Params]), JObj) of
        {'ok', Results} -> Results;
        {'error', Reason} -> knm_errors:by_carrier(?MODULE, Reason, Num)
    end.

-spec search_tollfree(nonempty_string(), [nonempty_string()], kz_json:object()) -> kz_json:object().
search_tollfree(Num, Params, JObj) ->
    case knm_thinq_util:api_post(lists:flatten(
                    [url_search("/origination/did/search/tollfree/")
                     ,"?" 
                     ,Params]), JObj) of
        {'ok', Results} -> Results;
        {'error', Reason} -> knm_errors:by_carrier(?MODULE, Reason, Num)
    end.

-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.

-spec quantity_uri_param(integer()) -> string().
quantity_uri_param(Q) -> integer_to_list(min(Q, ?MAX_SEARCH_QUANTITY)).
