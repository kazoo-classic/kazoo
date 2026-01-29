%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Check if a callback schedule is enabled on a voicemailbox and start a callback scheduler
%%% @author Harenson Henao
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_schedule_callback).

-export([init/0]).

%% Triggerables
-export([maybe_schedule_callback/2]).

-include("tasks.hrl").

-define(CATEGORY, "account_crawler").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok' | {'error', 'exists'}.
init() ->
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY>>
                           ,?MODULE
                           ,'maybe_schedule_callback'
                           ).

%% Triggerables
-spec maybe_schedule_callback(kz_term:ne_binary(), kzd_accounts:doc()) -> 'ok'.
maybe_schedule_callback(AccountId, _AccountJObj) ->
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:get_results(AccountDb, <<"vmboxes/listing_by_schedule">>, []) of
        {'ok', JObjs} ->
            lager:info("listing_by_schedule of account ~s returned: ~p", [AccountId, JObjs]),
            lists:foreach(fun load_schedule/1, lists:map(fun(J) ->
                                                                 {AccountId
                                                                 ,kz_json:get_value(<<"id">>, J)
                                                                 ,kz_json:get_value(<<"value">>, J)}
                                                         end,
                                                         JObjs));
        {'error', _E} ->
            lager:warning("account: ~s failed to load view vmboxes/listing_by_schedule : ~p", [AccountId, _E])
    end,
    'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
load_schedule({AccountId, Id, Schedule0}) ->
    %%    Schedule1 = kz_json:set_value([<<"action">>, <<"account_id">>], AccountId, Schedule0),
    %%    Schedule = kz_json:set_value([<<"action">>, <<"vmbox_id">>], Id, Schedule1),
    Routines = [fun (S) -> kz_json:set_value([<<"action">>, <<"account_id">>], AccountId, S) end
               ,fun (S) -> kz_json:set_value([<<"action">>, <<"vmbox_id">>], Id, S) end
               ],
    Schedule = lists:foldl(fun(F,P) -> F(P) end, Schedule0, Routines),

    ananke_maintenance:load_schedule(<<"vm_callback_", Id/binary>>, Schedule).
