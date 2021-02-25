%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications
%%% @doc CNAM lookup using Thinq
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_cnam_thinq).

-export([request/2]).

-include("stepswitch.hrl").

-define(DEFAULT_URL, <<"https://api.thinq.com/cnam/{{phone_number}}?format=json">>).

-define(HTTP_ACCEPT_HEADER, "application/json").
-define(HTTP_CONTENT_TYPE, "application/json").


-define(CNAM_HTTP_URL(AccountId, ResellerId)
       ,kapps_account_config:get_ne_binary(AccountId, ?CNAM_CONFIG_CAT, <<"http_url">>, ?CNAM_HTTP_URL(ResellerId))
       ).
    
-define(CNAM_HTTP_URL(AccountId)
       ,kapps_account_config:get_ne_binary(AccountId, ?CNAM_CONFIG_CAT, <<"http_url">>, ?DEFAULT_URL)
       ).

-define(CNAM_AUTH_USERNAME(AccountId, ResellerId)
       ,kz_term:to_list(kapps_account_config:get_ne_binary(AccountId, ?CNAM_CONFIG_CAT, <<"http_basic_auth_username">>, ?CNAM_AUTH_USERNAME(ResellerId)))
       ).
    
-define(CNAM_AUTH_USERNAME(AccountId)
       ,kapps_account_config:get_ne_binary(AccountId, ?CNAM_CONFIG_CAT, <<"http_basic_auth_username">>, <<>>)
       ).
    
-define(CNAM_AUTH_PASSWORD(AccountId, ResellerId)
       ,kz_term:to_list(kapps_account_config:get_ne_binary(AccountId, ?CNAM_CONFIG_CAT, <<"http_basic_auth_password">>, ?CNAM_AUTH_PASSWORD(ResellerId)))
       ).
    
-define(CNAM_AUTH_PASSWORD(AccountId)
       ,kapps_account_config:get_ne_binary(AccountId, ?CNAM_CONFIG_CAT, <<"http_basic_auth_password">>, <<>>)
       ).

-spec request(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
request(Number, JObj) ->
    Url = kz_term:to_list(get_http_url(JObj)),
    case kz_http:get(Url, get_http_headers(JObj), get_http_options()) of
        {'ok', 404, _, _} ->
            lager:debug("cnam lookup for ~s returned 404", [Number]),
            'undefined';
        {'ok', Status, _, <<>>} ->
            lager:debug("cnam lookup for ~s returned as ~p and empty body", [Number, Status]),
            'undefined';
        {'ok', 200=Status, _, ResponseBody} ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            handle_ok_resp(ResponseBody);
        {'ok', Status, _, ResponseBody} ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            'undefined';
        {'error', _R} ->
            lager:debug("cnam lookup for ~s failed: ~p", [Number, _R]),
            'undefined'
    end.

-spec get_http_url(kz_json:object()) -> kz_term:ne_binary().
get_http_url(JObj) ->
    AccountId = kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    Template = ?CNAM_HTTP_URL(AccountId, kzd_accounts:reseller_id(AccountId)),
    {'ok', SrcUrl} = stepswitch_cnam:render(JObj, Template),
    iolist_to_binary(SrcUrl).

-spec get_http_headers(kz_json:object()) -> [{nonempty_string(), nonempty_string()}].
get_http_headers(JObj) ->
    Headers = [{"Accept", ?HTTP_ACCEPT_HEADER}
              ,{"User-Agent", ?HTTP_USER_AGENT}
              ,{"Content-Type", ?HTTP_CONTENT_TYPE}
              ],
    maybe_enable_auth(JObj, Headers).

-spec get_http_options() -> kz_term:proplist().
get_http_options() ->
    [{'connect_timeout', ?HTTP_CONNECT_TIMEOUT_MS}
    ,{'timeout', 1500}
    ].

-spec maybe_enable_auth(kz_json:object(), [{nonempty_string(), nonempty_string()}]) ->
                               [{nonempty_string(), nonempty_string()}].
maybe_enable_auth(JObj, Props) ->
    AccountId = kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    Username = ?CNAM_AUTH_USERNAME(AccountId, kzd_accounts:reseller_id(AccountId)),
    Password = ?CNAM_AUTH_PASSWORD(AccountId, kzd_accounts:reseller_id(AccountId)),
    case kz_term:is_empty(Username)
        orelse kz_term:is_empty(Password)
    of
        'true' -> Props;
        'false' -> [basic_auth(Username, Password) | Props]
    end.

-spec handle_ok_resp(kz_term:ne_binary()) -> kz_term:api_binary().
handle_ok_resp(ResponseBody) ->
    JObj = kz_json:decode(ResponseBody),
    case kz_json:get_binary_value(<<"cnam">>, JObj) of
        'undefined' -> 'undefined';
        CallerInformation -> maybe_truncate(CallerInformation)
    end.

-spec maybe_truncate(binary()) -> binary().
maybe_truncate(CallerInformation) when size(CallerInformation) > 18 ->
    kz_binary:truncate_right(CallerInformation, 18);
maybe_truncate(CallerInformation) -> CallerInformation.

-spec basic_auth(nonempty_string(), nonempty_string()) ->
                        {nonempty_string(), nonempty_string()}.
basic_auth(Username, Password) ->
    Encoded = base64:encode_to_string(Username ++ [$: | Password]),
    {"Authorization", lists:flatten(["Basic ", Encoded])}.
