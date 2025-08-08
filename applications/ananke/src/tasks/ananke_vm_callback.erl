%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2022, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Ilya Ashchepkov)
%%% @end
%%%-----------------------------------------------------------------------------
-module(ananke_vm_callback).

-export([init/0
        ,handle_req/2
        ,check/2
        ,has_unread/2
        ]).

-include("ananke.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec check(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
check(AccountId, VMBoxId) ->
    lager:info("checking vmbox ~p in ~p", [VMBoxId, AccountId]),
    case has_unread(AccountId, VMBoxId) of
        'false' -> lager:info("no unread messages");
        'true' ->
            lager:info("found unread messages"),
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            handle_req(kz_json:from_list([{<<"Account-ID">>, AccountId}
                                         ,{<<"Account-DB">>, AccountDb}
                                         ,{<<"Voicemail-Box">>, VMBoxId}
                                         ])
                      ,[{<<"skip_verification">>, 'true'}]
                      )
    end.

-spec has_unread(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
has_unread(AccountId, VMBoxId) ->
    {New, _} = kvm_messages:count_non_deleted(AccountId, VMBoxId),
    New > 0.

-spec handle_req(kz_json:object(), kz_term:proplist()) -> any().
handle_req(JObj, Props) ->
    'true' = props:get_value(<<"skip_verification">>, Props, 'false')
        orelse kapi_notifications:voicemail_saved_v(JObj),
    _ = kz_util:put_callid(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = kz_json:get_value(<<"Account-DB">>, JObj),
    VMBoxId = kz_json:get_value(<<"Voicemail-Box">>, JObj),
    lager:debug("handling new voicemail in ~s", [VMBoxId]),
    {'ok', VMBoxJObj} = kz_datamgr:open_cache_doc(AccountDb, VMBoxId),

    UserId = kzd_voicemail_box:owner_id(VMBoxJObj, AccountId),

    OptionsPath = [<<"notify">>, <<"callback">>],
    VMBoxNotifyList = kz_json:get_value(OptionsPath, VMBoxJObj, []),

    {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
    Realm = kzd_accounts:realm(AccountDoc),

    Mailbox = kz_json:get_value(<<"mailbox">>, VMBoxJObj),
    VMNumber = get_voicemail_number(AccountDb, Mailbox),

    Callbacks = lists:map(fun(VMBoxNotifyJObj) ->
                  #callback{
                            callback_number = kz_json:get_value(<<"number">>, VMBoxNotifyJObj)
                            ,is_callback_disabled = kz_json:get_boolean_value(<<"disabled">>, VMBoxNotifyJObj)
                            ,call_timeout = get_callback_timeout(VMBoxNotifyJObj)
                            ,schedule = get_schedule(VMBoxNotifyJObj)
                           }
                  end,
                  VMBoxNotifyList),

    StartArgs = #args{account_id = AccountId
                     ,user_id = UserId
                     ,vm_box_id = VMBoxId
                     ,vm_number = VMNumber
                     ,realm = Realm
                     ,callbacks = Callbacks
                     },
    maybe_start_caller(StartArgs).

-spec get_voicemail_number(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
get_voicemail_number(AccountDb, Mailbox) ->
    {'ok', Callflows} = kz_datamgr:get_results(AccountDb
                                              ,<<"callflows/crossbar_listing">>
                                              ,['include_docs']
                                              ),
    case [Cf || Cf <- Callflows, is_voicemail_cf(Cf)] of
        [] -> 'undefined';
        [VMCallflow | _] -> get_callflow_number(VMCallflow, Mailbox)
    end.

-spec is_voicemail_cf(kz_json:object()) -> boolean().
is_voicemail_cf(JObj) ->
    FlowJObj = get_cf_flow(JObj),
    IsFlow = kz_json:is_json_object(FlowJObj)
        andalso not kz_json:is_empty(FlowJObj),
    case {IsFlow
          andalso kz_json:get_value([<<"doc">>, <<"numbers">>], JObj, []) /= []
         ,IsFlow
          andalso kz_json:get_value(<<"module">>, FlowJObj) =:= <<"voicemail">>
              andalso kz_json:get_value([<<"data">>, <<"action">>], FlowJObj) =:= <<"check">>
         }
    of
        {'false', _} -> 'false';
        {'true', 'true'} -> 'true';
        _ -> is_voicemail_cf(FlowJObj)
    end.

-spec get_cf_flow(kz_json:object()) -> kz_term:api_object().
get_cf_flow(JObj) ->
    case kz_json:get_value([<<"children">>, <<"_">>], JObj) of
        'undefined' -> kz_json:get_value([<<"doc">>, <<"flow">>], JObj);
        FlowJObj -> FlowJObj
    end.

-spec get_callflow_number(kz_json:object(), kz_term:ne_binary()) -> kz_term:api_binary().
get_callflow_number(Callflow, _Mailbox) ->
    case kz_json:get_value([<<"doc">>, <<"numbers">>], Callflow, ['undefined']) of
        [] -> 'undefined';
        [Number | _] -> Number
    end.

-spec maybe_start_caller(#args{}) -> 'ok'.
maybe_start_caller(#args{callbacks = []
                         ,account_id = AccountId
                         ,vm_box_id = VMBoxId}) ->
    lager:warning("not starting callback, no callback numbers defined in Account: ~s Mailbox: ~s", [AccountId, VMBoxId]);
maybe_start_caller(#args{vm_number = 'undefined', account_id = AccountId}) ->
    lager:warning("not starting callback, cannot find voicemail number in account ~p", [AccountId]);
maybe_start_caller(StartArgs) ->
    start_caller(StartArgs).

-spec start_caller(#args{}) -> 'ok'.
start_caller(#args{ account_id = AccountId
                  ,vm_box_id = VMBoxId
                  ,callbacks = Callbacks
                  } = Args) ->
    lager:info("starting callback worker for account: ~s voicemail ~s", [AccountId, VMBoxId]),
    OriginateReqFun = fun(A, Queue, Number, Timeout) -> build_originate_req(A, Queue, Number, Timeout) end,
    CheckFun = {?MODULE, has_unread, [AccountId, VMBoxId]},

    WorkerId = VMBoxId,
    WorkerArgs = [Args, Callbacks, OriginateReqFun, CheckFun],
    ananke_tasks_sup:start_task(WorkerId, 'ananke_callback_worker', WorkerArgs).

-spec build_originate_req(#args{},  kz_term:api_binary(), kz_term:api_binary(), pos_integer()) -> {kz_term:api_binary(), kapps_call:call(), kz_term:proplist()}.
build_originate_req(#args{vm_number = VMNumber
                         ,account_id = AccountId
                         ,user_id = UserId
                         ,realm = Realm
                         }, Queue, CallbackNumber, Timeout) ->

    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    TargetCallId = create_call_id(),
    Setters = [{fun kapps_call:set_account_id/2, AccountId}
              ,{fun kapps_call:set_account_db/2, AccountDb}
              ,{fun kapps_call:set_call_id/2, TargetCallId}
              ,{fun kapps_call:set_owner_id/2, UserId}
              ,{fun kapps_call:set_authorizing_type/2, <<"ananke">>}
              ,{fun kapps_call:set_authorizing_id/2, UserId}
              ,{fun kapps_call:set_resource_type/2, <<"audio">>}
              ],

    Call = kapps_call:exec(Setters, kapps_call:new()),

    MsgId = kz_binary:rand_hex(4),

    CCVs = props:filter_undefined(
             [{<<"Account-ID">>, kapps_call:account_id(Call)}
             ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
             ,{<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)}
             ,{<<"Channel-Authorized">>, 'true'}
             ,{<<"Inherit-Codec">>, <<"false">>}
             ,{<<"Realm">>, Realm}
             ,{<<"Account-Realm">>, Realm}
             ,{<<"From-Realm">>, Realm}
             ,{<<"Format-From-URI">>, <<"true">>}
             ,{<<"From-URI-Realm">>, Realm}
             ]),

    Endpoint = kz_json:from_list(
                 props:filter_undefined(
                   [{<<"Invite-Format">>, <<"loopback">>}
                   ,{<<"Route">>,  CallbackNumber}
                   ,{<<"To-DID">>, CallbackNumber}
                   ,{<<"To-Realm">>, Realm}
                   ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                   ,{<<"Outbound-Call-ID">>, TargetCallId}
                   ])),

    {TargetCallId, Call, props:filter_undefined(
                [{<<"Endpoints">>, [Endpoint]}
                ,{<<"Outbound-Call-ID">>, TargetCallId}
                ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                ,{<<"Msg-ID">>, MsgId}
                ,{<<"Continue-On-Fail">>, 'true'}
                ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
                ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>
                                                     ,<<"Account-Realm">>
                                                     ,<<"Authorizing-ID">>
                                                     ,<<"Authorizing-Type">>
                                                     ,<<"Owner-ID">>
                                                    ]}
                ,{<<"Application-Name">>, <<"park">>}
                ,{<<"Timeout">>, Timeout}

                ,{<<"Resource-Type">>, <<"originate">>}
                ,{<<"Originate-Immediate">>, 'true'}
                 | kz_api:default_headers(Queue, ?APP_NAME, ?APP_VERSION)
                ])}.

-spec create_call_id() -> kz_term:ne_binary().
create_call_id() ->
    <<"ananke-call-", (kz_binary:rand_hex(4))/binary>>.

-spec get_schedule(kz_json:object()) -> pos_integers().
get_schedule(VMBoxJObj) ->
    case kz_json:get_value(<<"schedule">>, VMBoxJObj,[])
    of
        [_|_] = Schedule -> Schedule;
        [] ->
            Attempts = get_attempts(VMBoxJObj),
            Interval = get_interval(VMBoxJObj),
            get_schedule_from_attempts_interval(Attempts, Interval)
    end.

-spec get_schedule_from_attempts_interval(integer(), pos_integer()) -> pos_integers().
get_schedule_from_attempts_interval(Attempts, Interval)
  when is_integer(Attempts), is_integer(Interval), Interval > 0 ->
    lists:duplicate(Attempts, Interval);
get_schedule_from_attempts_interval(_Attempts, _Interval) -> [].

-spec get_interval(kz_json:object()) -> pos_integer().
get_interval(VMBoxJObj) ->
    case kz_json:get_value(<<"interval_s">>, VMBoxJObj)
    of
        undefined ->
            kapps_config:get_integer(?CONFIG_CAT
                                    ,[<<"voicemail">>, <<"notify">>, <<"callback">>, <<"interval_s">>]
                                    ,5 * ?SECONDS_IN_MINUTE
                                    );
        Interval -> kz_term:to_integer(Interval)
    end.

-spec get_attempts(kz_json:object()) -> pos_integer().
get_attempts(VMBoxJObj) ->
    case kz_json:get_value(<<"attempts">>, VMBoxJObj)
    of
        undefined ->
            kapps_config:get_integer(?CONFIG_CAT
                                    ,[<<"voicemail">>, <<"notify">>, <<"callback">>, <<"attempts">>]
                                    ,5
                                    );
        Tries -> kz_term:to_integer(Tries)
    end.

-spec get_callback_timeout(kz_json:object()) -> pos_integer().
get_callback_timeout(VMBoxJObj) ->
    case kz_json:get_value(<<"timeout_s">>, VMBoxJObj)
    of
        undefined ->
            kapps_config:get_integer(?CONFIG_CAT
                                    ,[<<"voicemail">>, <<"notify">>, <<"callback">>, <<"timeout_s">>]
                                    ,20
                                    );
        CallTimeout -> kz_term:to_integer(CallTimeout)
    end.
