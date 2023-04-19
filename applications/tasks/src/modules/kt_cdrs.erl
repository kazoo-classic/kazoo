%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_cdrs).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Triggerables
-export([dump_resellers_cdrs/0]).

-export([dump_resellers_cdrs/1]).

%% Appliers
-export([dump/2]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).
-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).

-define(CATEGORY, "billing").
-define(ACTIONS, [<<"dump">>]).

-define(ACC_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_DAILY, ?MODULE, 'dump_reseller_cdrs'),
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec dump_resellers_cdrs() -> 'ok'.
dump_resellers_cdrs() ->
    ResellerIds = kapps_config:get(?MOD_CAT, <<"reseller_ids">>, []),
    _P = kz_util:spawn(fun dump_resellers_cdrs/1, [ResellerIds]).

-spec dump_resellers_cdrs(kz_term:list()) -> 'ok'.
dump_resellers_cdrs(ResellerIds) ->
    lager:info("dump_resellers_cdrs ~p", [ResellerIds]),
    lists:foreach(fun dump_reseller_cdrs/1, ResellerIds),
    ok.

-spec cleanup(kz_term:ne_binary(), any()) -> any().
cleanup(?NE_BINARY, _) -> 'ok'.

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_map(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> map().
action(<<"dump">>) ->
    #{<<"description">> => <<"Create CDR CSV with all CDRs from the requested timeframe">>
     ,<<"doc">> => <<"Retrieving large CDR CSVs via API can be problematic, and finding the right time"
                     " frame to get successful pages is a moving target. This task allows KAZOO to build"
                     " a CSV of a requested timeframe in the background and can notify the client when"
                     " the task has completed instead."
                   >>
     ,<<"expected_content">> => 'undefined' % currently no input, eventually support custom time frames
     }.

%%% Appliers

-spec output_header(kz_term:ne_binary()) -> kz_term:ne_binaries().
output_header(<<"dump">>) ->
    [K || {K, _} <- kzd_cdrs:csv_headers('true')].

-spec dump(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump(#{'account_id' := AccountId}=_ExtraArgs, 'init') ->
    AccountDb = kz_util:format_account_mod_id(AccountId),
    lager:info("starting dump of ~s", [AccountDb]),

    process_rows(AccountDb, 'undefined');
dump(#{}=_ExtraArgs, {_AccountDb, 'undefined'}) ->
    'stop';
dump(#{}=_ExtraArgs, {AccountDb, NextStartKey}) ->
    process_rows(AccountDb, NextStartKey).

dump_reseller_cdrs(ResellerId) ->
    {ok, JObjs} = kz_datamgr:get_results(<<"accounts">>, ?ACC_VIEW_DESCENDANTS
                            ,[{'startkey', [ResellerId]}
                              ,{'endkey', [ResellerId, kz_json:new()]}
                             ]),
    OutputPath = output_path(ResellerId),
    Header = [K || {K, _} <- kzd_cdrs:csv_headers('true')],
    'ok' = file:write_file(OutputPath, row_to_iolist(Header), ['append']),
    Descendants = [{OutputPath, kz_doc:id(JObj)} || JObj <- JObjs],
    lists:foreach(fun dump_descendant_cdrs/1, Descendants).
 
dump_descendant_cdrs({OutputPath, AccountId}) ->
    AccountDb = kz_util:format_account_mod_id(AccountId),
    lager:info("starting dump of ~s", [AccountDb]),
    Data = dump_descendant_cdrs(AccountDb, 'init', []),
    'ok' = file:write_file(OutputPath, row_to_iolist(Data), ['append']).

dump_descendant_cdrs(AccountDb, 'init', Rows) ->
    {CDRRows, NewState} =  process_rows(AccountDb, 'undefined'),
    dump_descendant_cdrs(AccountDb, NewState, Rows ++ CDRRows);
dump_descendant_cdrs(_AccountDb, {_AccountDb, 'undefined'}, Rows) ->
    Rows;
dump_descendant_cdrs(AccountDb, State, Rows) ->
    {CDRRows, NewState} =  process_rows(AccountDb, State),
    dump_descendant_cdrs(AccountDb, NewState, Rows ++ CDRRows).

-spec process_rows(kz_term:ne_binary(), 'undefined' | {kz_term:ne_binary(), kz_json:api_json_term()}) ->
          {[iolist()] | kz_datamgr:data_error(), {kz_term:ne_binary(), kz_json:api_json_term()}}.
process_rows(AccountDb, 'undefined') ->
    case get_page(AccountDb, 'undefined') of
        {'ok', Rows, NextStartKey} ->
            lager:info("got ~p rows (next:~s)", [length(Rows), NextStartKey]),
            CDRRows = [kzd_cdrs:to_public_csv(CDR, 'true') || CDR <- rows_to_cdrs(Rows)],
            {CDRRows, {AccountDb, NextStartKey}};
        {'error', E} ->
            lager:error("error getting first page: ~p", [E]),
            {E, []}
    end;
process_rows(AccountDb, {AccountDb, StartKey}) ->
    case get_page(AccountDb, StartKey) of
        {'ok', Rows, NextStartKey} ->
            lager:info("got ~p rows from ~s (next:~s)", [length(Rows), StartKey, NextStartKey]),
            {[kzd_cdrs:to_public_csv(CDR, 'true') || CDR <- rows_to_cdrs(Rows)], {AccountDb, NextStartKey}};
        {'error', E} ->
            lager:error("error getting page from ~p: ~p", [E, StartKey]),
            {E, []}
    end.

rows_to_cdrs(Rows) ->
    [kz_json:get_json_value(<<"doc">>, Row)
     || Row <- Rows
    ].

-spec get_page(kz_term:ne_biary(), kz_json:api_json_term()) -> kz_datamgr:paginated_results().
get_page(AccountDb, 'undefined') ->
    query(AccountDb, [{'startkey', start_key()}, {'endkey', end_key()}]);
get_page(AccountDb, NextStartKey) ->
    query(AccountDb, [{'startkey', NextStartKey}, {'endkey', end_key()}]).

-spec query(kz_term:ne_binary(), kz_datamgr:view_options()) -> kz_datamgr:paginated_results().
query(AccountDb, ViewOptions) ->
    Options = ['include_docs'
              ,'ascending'
               | ViewOptions
              ],
    lager:info("kz_datamgr:paginate_results(~p, ~p, ~p)."
              ,[AccountDb, <<"cdrs/crossbar_listing">>, Options]
              ),
    kz_datamgr:paginate_results(AccountDb
                               ,<<"cdrs/crossbar_listing">>
                               ,Options
                               ).

-spec output_path(kz_term:ne_binary()) -> file:filename_all().
output_path(AccountId) ->
    {Yr,Mn,Dy} = yesterday(),
    D = list_to_binary(io_lib:format("~p-~p-~p", [Yr,Mn,Dy])),
    <<"/tmp/cdrs.", AccountId/binary, ".", D/binary, ".csv">>.

start_key() ->
    Yesterday = yesterday(),
    [calendar:datetime_to_gregorian_seconds({Yesterday, {0,0,0}}), kz_json:new()].

end_key() ->
    Yesterday = yesterday(),
    [calendar:datetime_to_gregorian_seconds({Yesterday, {23,59,59}})].

yesterday() ->
    {Date, _Time} = calendar:universal_time(),
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 1).

row_to_iolist([]) ->
    <<>>;
row_to_iolist(Row) ->
    kz_csv:row_to_iolist(Row).
