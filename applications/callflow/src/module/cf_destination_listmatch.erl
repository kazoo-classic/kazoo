%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2025, 2600Hz
%%% @doc Checks destination number against a list and branches to a child
%%% callflow based on match/no-match.
%%%
%%% ```
%%% {
%%%     data: {
%%%         "id": {ID of number list}
%%%     },
%%%     "children": {
%%%         "match": {next callflow node},
%%%         "nomatch": {next callflow node},
%%%     }
%%% }
%%% ```
%%%
%%% @author Kozlov Yakov (code from cf_cidlistmatch.erl)
%%% @author Ruel Tmeizeh [www.ruhnet.co]
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_destination_listmatch).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Destination = case kapps_call:callee_id_number(Call) of
                      <<>> -> handle_no_match(Call);
                      Number -> knm_converters:normalize(Number, kapps_call:account_id(Call))
                  end,

    List = kz_json:get_ne_binary_value(<<"id">>, Data),
    AccountDb = kapps_call:account_db(Call),
    lager:debug("comparing destination number ~s with match list ~s entries in ~s", [Destination, List, AccountDb]),
    case is_matching_prefix(AccountDb, List, Destination)
        orelse is_matching_regexp(AccountDb, List, Destination)
    of
        'true' -> handle_match(Call);
        'false' -> handle_no_match(Call)
    end.

-spec is_matching_prefix(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_matching_prefix(AccountDb, ListId, Number) ->
    NumberPrefixes = build_keys(Number),
    Keys = [[ListId, X] || X <- NumberPrefixes],
    case kz_datamgr:get_results(AccountDb, <<"lists/match_prefix_in_list">>, [{'keys', Keys}]) of
        {'ok', [_ | _]} -> 'true';
        _ -> 'false'
    end.

-spec is_matching_regexp(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_matching_regexp(AccountDb, ListId, Number) ->
    case kz_datamgr:get_results(AccountDb, <<"lists/regexps_in_list">>, [{'key', ListId}]) of
        {'ok', Regexps} ->
            Patterns = [kz_json:get_value(<<"value">>, X)
                        || X <- Regexps,
                           X =/= null
                       ],
            match_regexps(Patterns, Number);
        _ ->
            'false'
    end.

-spec match_regexps(kz_term:binaries(), kz_term:ne_binary()) -> boolean().
match_regexps([Pattern | Rest], Number) ->
    case re:run(Number, Pattern) of
        {'match', _} -> 'true';
        'nomatch' -> match_regexps(Rest, Number)
    end;
match_regexps([], _Number) -> 'false'.

%% TODO: this function from hon_util, may be place it somewhere in library?
-spec build_keys(binary()) -> kz_term:binaries().
build_keys(<<"+", E164/binary>>) ->
    build_keys(E164);
build_keys(<<D:1/binary, Rest/binary>>) ->
    build_keys(Rest, D, [D]).

-spec build_keys(binary(), binary(), kz_term:binaries()) -> kz_term:binaries().
build_keys(<<D:1/binary, Rest/binary>>, Prefix, Acc) ->
    build_keys(Rest, <<Prefix/binary, D/binary>>, [<<Prefix/binary, D/binary>> | Acc]);
build_keys(<<>>, _, Acc) -> Acc.

%%------------------------------------------------------------------------------
%% @doc Handle "match" condition
%% @end
%%------------------------------------------------------------------------------
-spec handle_match(kapps_call:call()) -> 'ok'.
handle_match(Call) ->
    case is_callflow_child(<<"match">>, Call) of
        'true' -> 'ok';
        'false' -> cf_exe:continue(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Handle a caller id "no match" condition
%% @end
%%------------------------------------------------------------------------------
-spec handle_no_match(kapps_call:call()) -> 'ok'.
handle_no_match(Call) ->
    case is_callflow_child(<<"nomatch">>, Call) of
        'true' -> 'ok';
        'false' -> cf_exe:continue(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Check if the given node name is a callflow child
%% @end
%%------------------------------------------------------------------------------
-spec is_callflow_child(kz_term:ne_binary(), kapps_call:call()) -> boolean().
is_callflow_child(Name, Call) ->
    lager:debug("Looking for callflow child ~s", [Name]),
    case cf_exe:attempt(Name, Call) of
        {'attempt_resp', 'ok'} ->
            lager:debug("found callflow child"),
            'true';
        {'attempt_resp', {'error', _}} ->
            lager:debug("failed to find callflow child"),
            'false'
    end.
