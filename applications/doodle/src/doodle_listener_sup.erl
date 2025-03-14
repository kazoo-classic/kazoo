%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_listener_sup).

-behaviour(supervisor).

-include("doodle.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([worker/0]).
-export([init/1]).

-define(CHILDREN, [?WORKER_ARGS_TYPE('doodle_listener', [], 'temporary')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    {'ok', Pid} = supervisor:start_link({'local', ?SERVER}, ?MODULE, []),
    _ = init_workers(Pid),
    {'ok', Pid}.

%%------------------------------------------------------------------------------
%% @doc Random Worker.
%% @end
%%------------------------------------------------------------------------------
-spec worker() -> {'error', 'no_connections'} | pid().
worker() ->
    Listeners = supervisor:which_children(?SERVER),
    case length(Listeners) of
        0 -> {'error', 'no_connections'};
        Size ->
            Selected = rand:uniform(Size),
            {_, Pid, _, _} = lists:nth(Selected, Listeners),
            Pid
    end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

init_workers(Pid) ->
    Workers = kapps_config:get_integer(?CONFIG_CAT, <<"listeners">>, 1),
    _ = kz_util:spawn(fun() -> [begin
                                    _ = supervisor:start_child(Pid, []),
                                    timer:sleep(500)
                                end
                                || _N <- lists:seq(1, Workers)
                               ]
                      end).
