%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_sup).
-behaviour(supervisor).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/2, start_link/3, start_link/4
        ,listener/1
        ,fsm/1
        ,status/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER_ARGS_TYPE('acdc_agent_listener', [self() | Args], 'transient')
                  ,?WORKER_ARGS_TYPE('acdc_agent_fsm', [self() | Args], 'transient')
                  ]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kapps_call:call(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(ThiefCall, QueueId) ->
    supervisor:start_link(?SERVER, [ThiefCall, QueueId]).

-spec start_link(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_types:startlink_ret().
start_link(AcctId, AgentId, AgentJObj) ->
    supervisor:start_link(?SERVER, [AcctId, AgentId, AgentJObj]).

-spec start_link(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binaries()) -> kz_types:startlink_ret().
start_link(AcctId, AgentId, AgentJObj, Queues) ->
    supervisor:start_link(?SERVER, [AcctId, AgentId, AgentJObj, Queues]).

-spec status(pid()) -> 'ok'.
status(Supervisor) ->
    case {listener(Supervisor), fsm(Supervisor)} of
        {LPid, FSM} when is_pid(LPid), is_pid(FSM) ->
            {_AcctId, AgentId, _Q} = acdc_agent_listener:config(LPid),
            Status = acdc_agent_fsm:status(FSM),
            Aug_status = print_status(Status ++ augment_status(LPid)),
            ?PRINT(" ~32s |~s", [AgentId, Aug_status]);
        _ ->
            ?PRINT("Agent Supervisor ~p is dead", [Supervisor])
    end.

augment_status(LPid) ->
    Fs = ?AGENT_INFO_FIELDS,
    [{F, acdc_agent_listener:agent_info(LPid, F)} || F <- Fs].

print_status(Status) ->
    print_status(Status, []).

print_status([], Acc) -> lists:flatten(Acc);
print_status([{_, 'undefined'}|T], Acc) -> print_status(T, Acc);
print_status([{_K, V}|T], Acc) when is_binary(V) ->
    print_status(T, Acc ++ [io_lib:format(" ~20s |", [V])]);
print_status([{_K, V}|T], Acc) ->
    print_status(T, Acc ++ [io_lib:format(" ~20p |", [V])]).

-spec listener(pid()) -> kz_term:api_pid().
listener(Super) ->
    case child_of_type(Super, 'acdc_agent_listener') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec fsm(pid()) -> kz_term:api_pid().
fsm(Super) ->
    case child_of_type(Super, 'acdc_agent_fsm') of
        [] -> 'undefined';
        [P] -> P
    end.

-spec child_of_type(pid(), atom()) -> kz_term:pids().
child_of_type(S, T) -> [P || {Ty, P, 'worker', _} <- supervisor:which_children(S), T =:= Ty].

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init(Args) ->
    RestartStrategy = 'one_for_all',
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
