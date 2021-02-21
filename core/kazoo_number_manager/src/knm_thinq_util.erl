%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% @author Alan R Evans
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_thinq_util).

-export([api_get/1, api_get/2
        ,api_post/2, api_post/3]).
-export([to_thinq/1, from_thinq/1]).

-export([get_account_and_reseller_id/1]).

-include("knm.hrl").
-include("knm_thinq.hrl").

-type api_res() :: {'ok', kz_types:xml_el()} | {'error', atom()}.

-spec api_get(nonempty_string()) -> api_res().
-ifndef(TEST).
api_get(Url) ->
    api_get(Url, []).

-spec api_get(nonempty_string(), knm_search:options()) -> api_res().
api_get(Url, Options) ->
    HTTPOptions = [auth(Options)
                  ],
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n", ['get', Url, HTTPOptions]),
    Response = kz_http:get(Url, [], HTTPOptions),
    handle_response(Response).
-else.
api_get("https://api.inetwork.com/v1.0/accounts/eunit_testing_account/availableNumbers?areaCode="++_) ->
    Resp = knm_util:fixture("thinq_find_by_npa_no_detail.xml"),
    handle_response({'ok', 200, [], Resp});
api_get("https://api.inetwork.com/v1.0/accounts/eunit_testing_account/availableNumbers?tollFreeWildCardPattern="++_) ->
    Resp = knm_util:fixture("thinq_find_tollfree.xml"),
    handle_response({'ok', 200, [], Resp});
api_get("https://api.inetwork.com/v1.0/accounts/eunit_testing_account/orders/" ++ _) ->
    Resp = knm_util:fixture("thinq_check_order.xml"),
    handle_response({'ok', 200, [], Resp}).

-spec api_get(nonempty_string(), knm_search:options()) -> api_res().
api_get(Url, _) ->
    api_get(Url).
-endif.

-spec api_post(nonempty_string(), binary()) -> api_res().
api_post(Url, Body) ->
    api_post(Url, Body, []).
    
-spec api_post(nonempty_string(), binary(), knm_search:options()) -> api_res().
-ifndef(TEST).
api_post(Url, Body, Options) ->
    EncodeBody = kz_json:encode(Body),
    Headers = [{"Accept", "application/json"}
              ,{"User-Agent", ?KNM_USER_AGENT}
              ,{"Content-Type", "application/json"}
              ],
    HTTPOptions = [auth(Options)
                  ,{'ssl', [{'verify', 'verify_none'}]}
                  ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'body_format', 'string'}
                  ],
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n~p~n~p~p~n", ['post', Url, Headers, HTTPOptions, Body, EncodeBody]),
    Response = kz_http:post(Url, Headers, EncodeBody, HTTPOptions),
    handle_response(Response).
-else.
api_post("https://api.thinq.com/origination/did/search/individual/eunit_testing_account"++_, Body) ->
    _EncodeBody = kz_json:encode(Body),
    Resp = knm_util:fixture("thinq_search_number.json"),
    handle_response({'ok', 200, [], Resp});
api_post("https://api.thinq.com/origination/did/search/block/eunit_testing_account"++_, Body) ->
    _EncodeBody = kz_json:encode(Body),
    Resp = knm_util:fixture("thinq_search_block.json"),
    handle_response({'ok', 200, [], Resp});
api_post("https://api.thinq.com/origination/did/search/tollfree/eunit_testing_account"++_, Body) ->
    _EncodeBody = kz_json:encode(Body),
    Resp = knm_util:fixture("thinq_search_tollfree.json"),
    handle_response({'ok', 200, [], Resp});
api_post("https://api.thinq.com/account/eunit_testing_account/origination/order/create", _) ->
    Resp = knm_util:fixture("thinq_create_order.json"),
    handle_response({'ok', 201, [], Resp});
api_post("https://api.thinq.com/account/eunit_testing_account/origination/order/complete"++_, _) ->
    Resp = knm_util:fixture("thinq_create_order.json"),
    handle_response({'ok', 200, [], Resp}).

-endif.

%%------------------------------------------------------------------------------
%% @doc Make a REST request to Thinq.com Numbers API to perform the
%% given verb (purchase, search, provision, etc).
%% @end
%%------------------------------------------------------------------------------
-spec handle_response(kz_http:ret()) -> api_res().
handle_response({Result, Code, Props, Response})
  when is_binary(Response) ->
    handle_response({Result, Code, Props, kz_term:to_list(Response)});
handle_response({'ok', 400, _, Response}) ->
    ?DEBUG_APPEND("Response:~n400~n~s~n", [Response]),
    lager:debug("thinq.com request error: 400 (unauthenticated)"),
    {'error', reason(Response, kz_json:is_empty(Response))};

handle_response({'ok', 401, _, Response}) ->
    ?DEBUG_APPEND("Response:~n401~n~s~n", [Response]),
    lager:debug("thinq.com request error: 401 ~s", [Response]),
    {'error', reason(Response, kz_json:is_empty(Response))};

handle_response({'ok', 403, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n403~n~s~n", [_Response]),
    lager:debug("thinq.com request error: 403 (unauthorized)"),
    {'error', 'authorization'};

handle_response({'ok', 404, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n404~n~s~n", [_Response]),
    lager:debug("thinq.com request error: 404 (not found)"),
    {'error', 'not_found'};

handle_response({'ok', 500, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n500~n~s~n", [_Response]),
    lager:debug("thinq.com request error: 500 (server error)"),
    {'error', 'server_error'};

handle_response({'ok', 503, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n503~n~s~n", [_Response]),
    lager:debug("thinq.com request error: 503"),
    {'error', 'server_error'};

handle_response({'ok', 200=Code, Headers, Response}) ->
    ?DEBUG_APPEND("Response:~nCode : ~p~nBody : ~s~nHeaders : ~p~n", [Code, Response, Headers]),
    lager:debug("received response from thinq.com"),
    {ok, kz_json:decode(Response)};

handle_response({'ok', 201=Code, Headers, Response}) ->
    ?DEBUG_APPEND("Response:~nCode : ~p~nBody : ~s~nHeaders : ~p~n", [Code, Response, Headers]),
    lager:debug("received response from thinq.com"),
    {ok, kz_json:decode(Response)};

handle_response({'ok', Code, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~s~n", [Code, _Response]),
    lager:debug("thinq.com empty response: ~p", [Code]),
    {'error', 'empty_response'};

handle_response({'error', _}=E) ->
    lager:debug("thinq.com request error: ~p", [E]),
    E.

reason(_, 'true') ->
    'authentication';
reason(Reason, 'false') ->
    kz_json:decode(Reason).

-spec auth(knm_search:options()) -> {'basic_auth', {kz_term:ne_binary(), kz_term:ne_binary()}}.
auth([]) ->
    {'basic_auth', {?THQ_API_USERNAME, ?THQ_API_PASSWORD}};
auth(Options) ->
    Account_id = knm_carriers:account_id(Options),
    Reseller_id = knm_carriers:reseller_id(Options),
    {'basic_auth', {?THQ_API_USERNAME(Account_id, Reseller_id), ?THQ_API_PASSWORD(Account_id, Reseller_id)}}.

-spec to_thinq(kz_term:ne_binary()) -> kz_term:ne_binary().
to_thinq(<<"+1", Number/binary>>) -> Number;
to_thinq(Number) -> binary_to_integer(Number).

-spec from_thinq(kz_term:ne_binary()) -> kz_term:ne_binary().
from_thinq(<<"+", _/binary>> = Number) -> Number;
from_thinq(Number) -> <<"+1", Number/binary>>.

-spec get_account_and_reseller_id(knm_number:knm_number()) -> {'ok', kz_term:ne_binary(), kz_term:api_ne_binary()} |
          {'error', kz_term:ne_binary()}.
get_account_and_reseller_id(Number) ->
    case knm_phone_number:assigned_to(knm_number:phone_number(Number)) of
        'undefined' ->
            {'error', <<"number_unassigned">>};
        AccountId ->
            get_reseller_id(AccountId)
    end.

get_reseller_id(AccountId) ->
    {'ok', AccountId, kz_services_reseller:get_id(AccountId)}.
