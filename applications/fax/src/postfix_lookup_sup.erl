%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2025-2025, RuhNet
%%% @doc Supervisor for Postfix tcp_table server workers
%%%
%%% @author Ruel Tmeizeh (www.ruhnet.co)
%%% @end
%%%-----------------------------------------------------------------------------
-module(postfix_lookup_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("fax.hrl").

-define(SERVER, ?MODULE).

-define(POSTFIX_LOOKUP_DOMAIN_PORT, kapps_config:get_integer(?CONFIG_CAT, <<"postfix_lookup_domain_port">>, 19024)).
-define(POSTFIX_LOOKUP_TRANSPORT_PORT, kapps_config:get_integer(?CONFIG_CAT, <<"postfix_lookup_transport_port">>, 19023)).
-define(POSTFIX_WORKER_COUNT, kapps_config:get_integer(?CONFIG_CAT, <<"postfix_lookup_workers">>, 4)).

-define(WORKER_SPEC(Mod, Number, Args), {{Mod, Number},{Mod, 'start_link', Args}, 'permanent', 5000, 'worker', [Mod]}).

-spec children(gen_tcp:socket(), gen_tcp:socket()) -> kz_types:sup_child_specs().
children(LSocketDomain, LSocketTransport) ->
    lager:notice("postfix lookup starting ~p domain lookup workers on port ~p", [?POSTFIX_WORKER_COUNT, ?POSTFIX_LOOKUP_DOMAIN_PORT]),
    lager:notice("postfix lookup starting ~p transport lookup workers on port ~p", [?POSTFIX_WORKER_COUNT, ?POSTFIX_LOOKUP_TRANSPORT_PORT]),
    DomainWorkers = [?WORKER_SPEC('postfix_lookup_domain', N, [LSocketDomain]) || N <- lists:seq(1, ?POSTFIX_WORKER_COUNT)],
    TransportWorkers = [?WORKER_SPEC('postfix_lookup_transport', N, [LSocketTransport]) || N <- lists:seq(1, ?POSTFIX_WORKER_COUNT)],
    DomainWorkers ++ TransportWorkers.

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ServerOpts = [{'mode', 'binary'}
                 ,{'active', 'false'}
                 ,{'packet', 'line'}
                 ,{'reuseaddr', 'true'}
                 ],
    case kapps_config:get_is_true(?CONFIG_CAT, <<"postfix_lookup">>, 'false') of
        'false' -> {'ok', {SupFlags, []}}; %% do nothing and keep the supervisor happy
        'true' ->
            case {gen_tcp:listen(?POSTFIX_LOOKUP_DOMAIN_PORT, ServerOpts)
                 ,gen_tcp:listen(?POSTFIX_LOOKUP_TRANSPORT_PORT, ServerOpts)
                 }
            of %% start listening socket and share it among workers
                {{'ok', DSocket}, {'ok', TSocket}} -> {'ok', {SupFlags, children(DSocket, TSocket)}};
                {{'error', Reason}, _} -> {'stop', Reason};
                {_, {'error', Reason}} -> {'stop', Reason}
            end
    end.
