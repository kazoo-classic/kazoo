%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2025-2025, RuhNet
%%% @doc Postfix tcp_table server - implements Postfix client/server table
%%% lookup protocol, for lookup of fax domains, so that Postfix can determine
%%% the transport to use for them, whether to send them to Kazoo, or out to the
%%% internet via normal delivery.
%%% See https://www.postfix.org/tcp_table.5.html
%%%
%%% Request format is a single line: "get {key-to-lookup}\n"
%%% Response format is one of:
%%% not found response: "500 {not found message}\n"
%%% error response:     "400 {error message}\n"  (client will retry later)
%%% success response:   "200 {result}\n"
%%%
%%% @author Ruel Tmeizeh (www.ruhnet.co)
%%% @end
%%%-----------------------------------------------------------------------------
-module(postfix_lookup_transport).
-behavior(gen_server).

-export([start_link/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("fax.hrl").

-define(SERVER, ?MODULE).

-type state() :: gen_tcp:socket().

-spec start_link(gen_tcp:socket()) -> kz_types:startlink_ret().
start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

-spec init([gen_tcp:socket()]) -> {'ok', state()}.
init([LSocket]) ->
    gen_server:cast(self(), 'accept'),
    {'ok', LSocket}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("terminating ~p worker: ~p", [?MODULE, _Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('accept', LSocket=_State) ->
    _ = postfix_lookup_util:accept(LSocket, fun lookup_and_serve/2, ?MODULE), %% handle this request
    gen_server:cast(self(), 'accept'), %% ready for accepting the next request
    {'noreply', LSocket};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec lookup_and_serve(gen_tcp:socket(), kz_term:ne_binary()) -> 'ok' | {'error', any()}.
lookup_and_serve(Socket, ReqKey) ->
    SMTPPort = kz_term:to_binary(?SMTP_PORT),
    KZTransport = kapps_config:get_ne_binary(?CONFIG_CAT, <<"postfix_lookup_kazoo_transport">>, <<"smtp:[127.0.0.1]:", SMTPPort/binary>>),
    Res = case postfix_lookup_util:check_fax_domain(ReqKey) of
              'ok' ->
                  <<KZTransport/binary, "\n">>;
              'not_found' ->
                  <<"smtp:\n">>; %% send using regular SMTP transport out to internet (this is outbound mail)
              _ ->
                  <<"400 error\n">>
          end,
    gen_tcp:send(Socket, Res).
