%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Ilya Ashchepkov)
%%% @end
%%%-----------------------------------------------------------------------------
-module(ananke_callback_worker).
-behaviour(gen_listener).

-export([start_link/4
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,handle_call_event/2
        ,handle_originate_resp/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ananke.hrl").

-define(SERVER, ?MODULE).

-define(PRESS_ANY_KEY_PROMPT, kapps_config:get(?CONFIG_CAT, <<"press_any_key_prompt">>, <<"agent-logged_out">>)).

-record(state, {
               args             :: args()
               ,callbacks        :: callbacks()
               ,originate_fun :: fun()
               ,check = 'true' :: check_fun()
               ,call = 'undefined'
               ,call_id = 'undefined'
               ,confirmed = false :: boolean()
               ,callback_position :: pos_integer()
               ,originate_timer        :: kz_term:api_reference()
               ,confirm_timer = 'undefined' :: kz_term:api_reference()
               ,schedule     :: pos_integers() | 'undefined'
               ,my_q         :: kz_term:api_binary()
               }).

-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                     ,[{<<"call_event">>, <<"*">>}]}
                     ,{{?MODULE, 'handle_originate_resp'}
                     ,[{<<"resource">>, <<"*">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-spec start_link(args(), callbacks(), fun(), check_fun()) -> {'ok', pid()} | {'error', any()}.
start_link(Args, Callbacks, OriginateFun, CheckFun) ->
    start_link(#state{args = Args,
                      callbacks = Callbacks
                     ,originate_fun = OriginateFun
                     ,check = CheckFun
                     ,callback_position = 1
                     }).

-spec start_link(state()) -> {'ok', pid()} | {'error', any()}.
start_link(#state{} = State) ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ]  
                                     ,[State]).

-spec init([state()]) -> {'ok', state()}.
init([#state{callbacks = Callbacks, callback_position = Pos} = State]) ->
    Callback = lists:nth(Pos, Callbacks),
    #callback{schedule = [Interval|Schedule]} = Callback,
    Timer = start_originate_timer(Callback, Interval * ?MILLISECONDS_IN_SECOND),
    {'ok', State#state{originate_timer = Timer, schedule = Schedule}}.

-spec handle_call(any(), any(), state()) -> {'noreply', state()}.
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    {'noreply', State#state{my_q = QueueName}};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'ananke_call_play_msg', _JObj}, #state{call = Call
                                                   } = State) ->
    kapps_call_command:prompt_and_collect_digit(?PRESS_ANY_KEY_PROMPT, Call),
    {'noreply', maybe_set_confirm_timer(State)};
handle_cast({'ananke_call_confirmed', _JObj}, #state{args = Args, confirm_timer = Timer, call = Call} = State) ->
    stop_confirm_timer(Timer),
    kapps_call_command:blind_transfer(Args#args.vm_number, Call),
    {'noreply', State#state{confirm_timer = 'undefined', confirmed = true}};
handle_cast({'ananke_originate_uuid', JObj}, #state{call = Call} = State) ->
    CtrlQ = kz_json:get_value(<<"Outbound-Call-Control-Queue">>, JObj),
    CallId = kz_json:get_value(<<"Outbound-Call-ID">>, JObj),
    lager:info("updating ~s with ~s", [CallId, CtrlQ]),
    {'noreply', State#state{call = kapps_call:set_control_queue(CtrlQ, Call)}};
handle_cast({'ananke_call_hungup', _JObj}, #state{call_id = CallId, confirmed = true} = State) ->
    lager:info("confirmed call hungup, unbind from call events ~s and stop", [CallId]),
    unbind_from_call_events(CallId),
    {'stop', 'normal', State};
handle_cast({'ananke_call_hungup', _JObj}, #state{call_id = CallId, confirmed = false} = State) ->
    lager:info("unconfirmed call hungup, unbind from call events ~s and continue", [CallId]),
    unbind_from_call_events(CallId),
    {'noreply', maybe_set_originate_timer(State)};
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec handle_info({'originate', callback()}, state()) -> {'noreply', state()} | {'stop', any(), state()}.
handle_info({'originate', #callback{callback_number = Number, call_timeout = Timeout}}, #state{args = Args, my_q = MyQ, originate_fun = OrigFun} = State) ->
    {CallId, Call, Req} = OrigFun(Args, MyQ, Number, Timeout),
    Routines = [{fun check_condition/2, {}}
               ,{fun maybe_bind_to_call_events/2, Req}
               ,{fun send_request/2, Req}
               ],
    return(State#state{originate_timer = 'undefined', call_id = CallId, call = Call}, Routines);
handle_info('confirm_timeout', #state{call_id = CallId, call = Call} = State) ->
    lager:info("confirm timeout, hangup ~s and continue", [CallId]),
    kapps_call_command:hangup(Call),
    {'noreply', State#state{confirm_timer = 'undefined'}};
handle_info(_Info, State) ->
    lager:info("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', [{pid, self()}]}.

-spec handle_call_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    _ = kz_util:put_callid(JObj),
    case kapi_call:event_v(JObj) of
        'true' ->
            {Category, Name} = kz_util:get_event_type(JObj),
            handle_call_event(Category, Name, JObj, Props);
        'false' ->
            'true' = kz_api:error_resp_v(JObj),
            ok
    end.

handle_call_event(<<"call_event">>, <<"CHANNEL_ANSWER">>, JObj, Props) ->
    lager:info("Call has been answered play recording"),
    Pid = props:get_value(pid, Props),
    gen_listener:cast(Pid, {'ananke_call_play_msg', JObj});
handle_call_event(<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, JObj, _Props) ->
    App = kz_json:get_value(<<"Application-Name">>, JObj),
    lager:debug("Ignoring CHANNEL_EXECUTE_COMPLETE event ~s", [App]);
handle_call_event(<<"call_event">>, <<"CHANNEL_DESTROY">>, JObj, Props) ->
    lager:info("Call ended"),
    Pid = props:get_value(pid, Props),
    gen_listener:cast(Pid, {'ananke_call_hungup', JObj});
handle_call_event(<<"call_event">>, <<"DTMF">>, JObj, Props) ->
    lager:info("Callee confirmed answered"),
    Pid = props:get_value(pid, Props),
    gen_listener:cast(Pid, {'ananke_call_confirmed', JObj});
handle_call_event(Category, Name, _JObj, _Props) ->
    lager:debug("unhandled call event: ~p:~p", [Category, Name]).

-spec terminate(any(), state()) -> 'ok'.
terminate(_, #state{originate_timer = Timer}) ->
    _ = stop_originate_timer(Timer),
    %% supervisor doesn't delete stopped child specification
    ananke_tasks_sup:delete_child(self(), 1 * ?MILLISECONDS_IN_SECOND),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-type routine_ret() :: state() | {'stop', any(), state()} | 'stop' | 'continue'.
-type routine_fun() :: fun((state(), any()) -> routine_ret()).
-type routine() :: {routine_fun(), any()}.
-type routines() :: [routine()].

-spec return(state(), routines()) -> {'stop', any(), state()} | {'noreply', state()}.
return(#state{} = State, [{Fun, Args} | Routines]) ->
    case Fun(State, Args) of
        #state{} = NewState ->
            return(NewState, Routines);
        'stop' -> {'stop', 'normal', State};
        {'stop', _, #state{}} = Return ->
            Return;
        'continue' ->
            return(State, Routines)
    end;
return(#state{} = NewState, []) ->
    {'noreply', NewState}.

-spec check_condition(state(), any()) -> routine_ret().
check_condition(#state{check = 'true'}, _) ->
    'continue';
check_condition(#state{check = {Module, Fun, Args}}, _) ->
    case erlang:apply(Module, Fun, Args) of
        'true' -> 'continue';
        'false' ->
            lager:info("condition failed, stopping"),
            'stop'
    end;
check_condition(#state{check = Fun}, _) when is_function(Fun, 0) ->
    case Fun() of
        'true' -> 'continue';
        'false' ->
            lager:info("condition failed, stopping"),
            'stop'
    end.

-spec maybe_bind_to_call_events(state(), kz_term:proplist()) -> routine_ret().
maybe_bind_to_call_events(_State, Req) ->
    CallId = props:get_value(<<"Outbound-Call-ID">>, Req),
    case bind_to_call_events(CallId) of
        'ok' -> 'continue';
        _  ->
            lager:warning("failed to bind to call events, stopping"),
            'stop'

    end.

-spec send_request(state(), kz_term:proplist()) -> routine_ret().
send_request(State, Req) ->
    lager:info("sending originate request"),
    kapi_resource:publish_originate_req(Req),
    State.

-spec maybe_set_originate_timer(state()) -> state().
maybe_set_originate_timer(#state{originate_timer = Timer} = State) when is_reference(Timer) ->
    lager:debug("originate timer already set, ignoring"),
    State;
maybe_set_originate_timer(#state{schedule = [TimeoutS | Schedule], callbacks = Callbacks, callback_position = Pos} = State) ->
    Callback = lists:nth(Pos, Callbacks),
    Timer = start_originate_timer(Callback, TimeoutS * ?MILLISECONDS_IN_SECOND),
    State#state{originate_timer = Timer, schedule = Schedule};
maybe_set_originate_timer(#state{schedule = [], callbacks = Callbacks, callback_position = Pos} = State) ->
    NextPos = 
    case Pos == length(Callbacks) of
        'true' ->  1;
        'false' -> Pos + 1
    end,
    Callback = lists:nth(NextPos, Callbacks),
    #callback{schedule = [Interval|Schedule]} = Callback,
    Timer = start_originate_timer(Callback, Interval * ?MILLISECONDS_IN_SECOND),
    State#state{originate_timer = Timer, schedule = Schedule, callback_position = NextPos}.

-spec start_originate_timer(callback(), integer()) -> reference().
start_originate_timer(Callback, Timeout) ->
    lager:info("scheduled callback ~p timer ~pms", [Callback, Timeout]),
    erlang:send_after(Timeout, self(), {'originate', Callback}).

-spec stop_originate_timer(reference()) -> 'ok' | {'error', any()}.
stop_originate_timer('undefined') -> 'ok';
stop_originate_timer(Timer) ->
    erlang:cancel_timer(Timer).

-spec maybe_set_confirm_timer(state()) -> state().
maybe_set_confirm_timer(#state{confirm_timer = Timer} = State) when is_reference(Timer) ->
    lager:debug("confirm timer already set, ignoring"),
    State;
maybe_set_confirm_timer(State) ->
    Timer = start_confirm_timer(10 * ?MILLISECONDS_IN_SECOND),
    State#state{confirm_timer = Timer}.

-spec start_confirm_timer(integer()) -> reference().
start_confirm_timer(Timeout) ->
    lager:info("confirm timer ~pms", [Timeout]),
    erlang:send_after(Timeout, self(), 'confirm_timeout').

-spec stop_confirm_timer(reference()) -> 'ok' | {'error', any()}.
stop_confirm_timer('undefined') -> 'ok';
stop_confirm_timer(Timer) ->
    erlang:cancel_timer(Timer).

-spec handle_originate_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_originate_resp(JObj, Props) ->
    lager:debug("handle_originate_resp: ~p", [kz_json:get_value(<<"Event-Name">>, JObj)]),
    Pid = props:get_value(pid, Props),
    case kz_json:get_value(<<"Event-Name">>, JObj) of
        <<"originate_resp">> ->
            'true' = kapi_resource:originate_resp_v(JObj),
            gen_listener:cast(Pid, {'ananke_originate_resp', JObj});
        <<"originate_started">> ->
            'true' = kapi_resource:originate_started_v(JObj),
            gen_listener:cast(Pid, {'ananke_originate_started', JObj});
        <<"originate_uuid">> ->
            'true' = kapi_resource:originate_uuid_v(JObj),
            gen_listener:cast(Pid, {'ananke_originate_uuid', JObj})
    end.

%% Handles subscribing/unsubscribing from call events
-spec bind_to_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call()) -> 'ok'.
bind_to_call_events(Call) ->
    bind_to_call_events(Call, self()).

-spec bind_to_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call(), pid()) ->  'undefined' | 'ok'.
bind_to_call_events('undefined', _) -> 'undefined';
bind_to_call_events(?NE_BINARY = CallId, Pid) ->
    gen_listener:add_binding(Pid, 'call', [{'callid', CallId}]);
bind_to_call_events({CallId, _}, Pid) -> bind_to_call_events(CallId, Pid);
bind_to_call_events(Call, Pid) -> bind_to_call_events(kapps_call:call_id(Call), Pid).

-spec unbind_from_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call()) -> 'ok'.
unbind_from_call_events(Call) ->
    unbind_from_call_events(Call, self()).

-spec unbind_from_call_events(kz_term:api_binary() | {kz_term:api_binary(), any()} | kapps_call:call(), pid()) -> 'ok'.
unbind_from_call_events('undefined', _Pid) -> 'ok';
unbind_from_call_events(?NE_BINARY = CallId, Pid) ->
    gen_listener:rm_binding(Pid, 'call', [{'callid', CallId}]);
unbind_from_call_events({CallId, _}, Pid) -> unbind_from_call_events(CallId, Pid);
unbind_from_call_events(Call, Pid) -> unbind_from_call_events(kapps_call:call_id(Call), Pid).

