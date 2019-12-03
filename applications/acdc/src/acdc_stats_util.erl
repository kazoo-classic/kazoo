%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%% Stat util functions
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_stats_util).

-export([wait_time/2
        ,pause_time/2
        ,caller_id_name/2
        ,caller_id_number/2

        ,get_query_limit/1
        ,db_name/1
        ,prev_modb/1

        ,cleanup_old_stats/0
        ,cleanup_old_calls/1, cleanup_old_statuses/1
        ]).

-include("acdc.hrl").
-include("acdc_stats.hrl").

-spec wait_time(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_integer().
wait_time(<<"paused">>, _) -> 'undefined';
wait_time(_, JObj) -> kz_json:get_integer_value(<<"Wait-Time">>, JObj).

-spec pause_time(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_integer().
pause_time(<<"paused">>, JObj) ->
    case kz_json:get_integer_value(<<"Pause-Time">>, JObj) of
        'undefined' -> kz_json:get_integer_value(<<"Wait-Time">>, JObj);
        PT -> PT
    end;
pause_time(_, _JObj) -> 'undefined'.

-spec caller_id_name(any(), kz_json:object()) -> api_kz_term:ne_binary().
-spec caller_id_number(any(), kz_json:object()) -> kz_term:api_integer().
caller_id_name(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Name">>, JObj).
caller_id_number(_, JObj) ->
    kz_json:get_value(<<"Caller-ID-Number">>, JObj).

-spec get_query_limit(kz_json:object()) -> pos_integer() | 'no_limit'.
get_query_limit(JObj) ->
    get_query_limit(JObj, ?STATS_QUERY_LIMITS_ENABLED).

-spec get_query_limit(kz_json:object(), boolean()) -> pos_integer() | 'no_limit'.
get_query_limit(JObj, 'true') ->
    Max = ?MAX_RESULT_SET,
    case kz_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> Max;
        N when N > Max -> Max;
        N when N < 1 -> 1;
        N -> N
    end;
get_query_limit(JObj, 'false') ->
    case kz_json:get_integer_value(<<"Limit">>, JObj) of
        'undefined' -> 'no_limit';
        N when N < 1 -> 1;
        N -> N
    end.

-spec db_name(kz_term:ne_binary()) -> kz_term:ne_binary().
db_name(Account) ->
    kz_util:format_account_mod_id(Account).

-spec prev_modb(kz_term:ne_binary()) -> kz_term:ne_binary().
prev_modb(Account) ->
    {{Year, Month, _}, _} = calendar:now_to_universal_time(os:timestamp()),
    prev_modb(Account, Year, Month-1).

-spec prev_modb(kz_term:ne_binary(), calendar:year(), integer()) -> kz_term:ne_binary().
prev_modb(Account, Year, 0) ->
    prev_modb(Account, Year-1, 12);
prev_modb(Account, Year, Month) ->
    kz_util:format_account_id(Account, Year, Month).

-spec cleanup_old_stats() -> 'ok'.
cleanup_old_stats() ->
    cleanup_old_calls(1200),
    cleanup_old_statuses(14400).

-spec cleanup_old_calls(pos_integer()) -> 'ok'.
cleanup_old_calls(Window) ->
    acdc_stats:manual_cleanup_calls(Window).

-spec cleanup_old_statuses(pos_integer()) -> 'ok'.
cleanup_old_statuses(Window) ->
    acdc_stats:manual_cleanup_statuses(Window).
