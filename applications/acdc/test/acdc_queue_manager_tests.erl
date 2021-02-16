%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016, Voxter Communications Inc.
%%% @doc
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_manager_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/acdc.hrl").
-include("../src/acdc_queue_manager.hrl").

-define(AGENT_ID, <<"agent_id">>).

%%% =====
%%% TESTS
%%% =====

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
ss_size_empty_test_() ->
    SS = #strategy_state{agents=pqueue4:new()},
    [?_assertEqual(0, acdc_queue_manager:ss_size('rr', SS, 'free'))
    ,?_assertEqual(0, acdc_queue_manager:ss_size('rr', SS, 'logged_in'))].

ss_size_one_busy_test_() ->
    SS = #strategy_state{agents=[]},
    State = #state{strategy='mi', strategy_state = SS},
    SS1 = acdc_queue_manager:update_strategy_with_agent(State, ?AGENT_ID, 0, [], 'add', 'undefined'),
    SS2 = acdc_queue_manager:update_strategy_with_agent(State#state{strategy_state = SS1}, ?AGENT_ID, 0, [], 'remove', 'busy'),
    [?_assertEqual(0, acdc_queue_manager:ss_size('mi', SS2, 'free'))
    ,?_assertEqual(1, acdc_queue_manager:ss_size('mi', SS2, 'logged_in'))].
