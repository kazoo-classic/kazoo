%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2025, 2600Hz
%%% @doc Put the call on hold
%%% Data = {
%%%    "moh_aleg": {indicator selection} % silence, pulse, beep, beep{frequency}, tone_stream://{tones pattern}
%%%   ,"moh_bleg": {media_id OR indicator selection, shout://{mp3 stream}, local_stream://{local stream}}
%%%   ,"unhold_key": {DTMF digit}
%%%   ,"update_channel": true/false % update the channel properties for 'is_onhold'
%%%   ,"unhold": true/false % if true, performs an un-hold action (using the FS 'break' command)
%%% }
%%%
%%% @author James Aimonetti
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% @author Ruel Tmeizeh (www.ruhnet.co)
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_hold).

-export([handle/2
        ,number_builder/1
        ]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> {'continue', kapps_call:call()}.
handle(Data, Call) ->
    case kz_json:is_true(<<"unhold">>, Data) of
        'true' -> handle_unhold(Data, Call);
        _ -> handle_hold(Data, Call)
    end.

-spec handle_unhold(kz_json:object(), kapps_call:call()) -> {'continue', kapps_call:call()}.
handle_unhold(Data, Call) ->
    RequestingLeg = kz_json:get_ne_binary_value(<<"dtmf_leg">>, Data),

    lager:debug("leg ~s is unholding the soft hold of ~s using 'break'", [RequestingLeg, hold_leg(Call, RequestingLeg)]),

    Command = [{<<"Application-Name">>, <<"break">>}
              ,{<<"Call-ID">>, RequestingLeg}
              ,{<<"Insert-At">>, <<"now">>}
              ],
    kapps_call_command:send_command(Command, Call),

    %% create and send an unhold event
    Props = generate_event(<<"CHANNEL_UNHOLD">>, Call),
    _ = kz_amqp_worker:cast(Props, fun kapi_call:publish_event/1),

    %% Update channel property to 'is_onhold: false', only if it is specified
    %% in the metaflow object.
    case kz_json:is_true(<<"update_channel">>, Data) of
        'false' -> {'continue', Call};
        'true' ->
            UpdateProps = [{<<"Call-ID">>, RequestingLeg}
                          ,{<<"Updates">>, kz_json:set_values([{<<"is_onhold">>, 'false'}], kz_json:new())}
                           | kz_api:default_headers(?APP_NAME, ?APP_VERSION) % E: undefined macro 'APP_NAME'
                          ],
            _ = kz_amqp_worker:cast(UpdateProps, fun kapi_call:publish_channel_update_req/1),
            {'continue', Call}
    end.

-spec handle_hold(kz_json:object(), kapps_call:call()) -> {'continue', kapps_call:call()}.
handle_hold(Data, Call) ->
    %% Using media for the A leg will unhold when file playback finishes! So we can't allow that;
    %% instead, we'll play silence or tones:
    AMOHToPlay = case kz_json:get_ne_binary_value(<<"moh_aleg">>, Data) of
                     <<"silen", _/binary>> -> ?SILENCE; % matches silence/silent as well as silence_stream://x
                     <<"pulse">> -> <<"tone_stream://v=-25%(2000,0,432,428.8);loops=-1">>;
                     <<"beep">> -> ?DEFAULT_BEEP;
                     <<"beep", Freq/binary>> -> <<"tone_stream://%(100,15000,", Freq/binary, ");loops=-1">>; % configurable beep
                     <<"tone_stream://", _/binary>>=Tones -> Tones; % allow arbitrary tones to be specified
                     _ -> ?DEFAULT_BEEP
                 end,

    %% Get hold media for the B-leg.
    BMOH = kz_json:get_ne_first_defined([<<"moh_bleg">>, <<"moh">>], Data),
    BMOHToPlay = konami_util:moh(BMOH, Call),

    %% NOTE that no unhold event will be fired if the user presses
    %% the unhold_key, and the channel cannot be updated.
    UnholdKey = kz_json:get_ne_binary_value(<<"unhold_key">>, Data, <<"1">>),

    RequestingLeg = kz_json:get_ne_binary_value(<<"dtmf_leg">>, Data),

    HoldCommand = kapps_call_command:soft_hold_command(RequestingLeg, UnholdKey, AMOHToPlay, BMOHToPlay),

    lager:debug("leg ~s is putting ~s on soft hold", [RequestingLeg, hold_leg(Call, RequestingLeg)]),

    kapps_call_command:send_command(props:set_value(<<"Insert-At">>, <<"now">>, HoldCommand)
                                   ,Call
                                   ),

    %% create and send a hold event. Note that the Ecallmanager channel
    %% state is not updated to on_hold:true. If it was, and the user pressed
    %% the unhold key, then the channel would be stuck in a held state as
    %% far as Ecallmanager knows, even though the call is no longer on hold.
    Props = generate_event(<<"CHANNEL_HOLD">>, Call),
    _ = kz_amqp_worker:cast(Props, fun kapi_call:publish_event/1),

    %% Update channel property to is_onhold: true, only if the request came
    %% from crossbar or was manually specified to update in metaflow object.
    _ = case kz_json:is_true(<<"update_channel">>, Data) of
            'false' -> 'ok';
            'true' ->
                UpdateProps = [{<<"Call-ID">>, RequestingLeg}
                              ,{<<"Updates">>, kz_json:set_values([{<<"is_onhold">>, 'true'}], kz_json:new())}
                               | kz_api:default_headers(?APP_NAME, ?APP_VERSION) % E: undefined macro 'APP_NAME'
                              ],
                _ = kz_amqp_worker:cast(UpdateProps, fun kapi_call:publish_channel_update_req/1)
        end,
    {'continue', Call}.

-spec hold_leg(kapps_call:call(), kz_term:ne_binary()) -> kz_term:ne_binary().
hold_leg(Call, RequestingLeg) when is_binary(RequestingLeg) ->
    case kapps_call:call_id(Call) of
        RequestingLeg -> kapps_call:other_leg_call_id(Call);
        HoldLeg -> HoldLeg
    end.

-spec number_builder(kz_json:object()) -> kz_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'hold' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'hold'? ", "~d"),

    K = [<<"numbers">>, kz_term:to_binary(Number)],

    case number_builder_check(kz_json:get_value(K, DefaultJObj)) of
        'undefined' -> kz_json:delete_key(K, DefaultJObj);
        NumberJObj -> kz_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(kz_term:api_object()) -> kz_term:api_object().
number_builder_check('undefined') ->
    number_builder_moh(kz_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [kz_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(kz_json:object(), string()) -> kz_term:api_object().
number_builder_check_option(NumberJObj, "e") ->
    number_builder_moh(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec number_builder_moh(kz_json:object()) -> kz_json:object().
number_builder_moh(NumberJObj) ->
    {'ok', [MOH]} = io:fread("Any custom music on hold to play ('n' to leave as default MOH, 'h' for help)? ", "~s"),
    metaflow_jobj(NumberJObj, MOH).

-spec metaflow_jobj(kz_json:object(), string()) -> kz_json:object().
metaflow_jobj(NumberJObj, "h") ->
    io:format("To set a system_media file as MOH, enter: /system_media/{MEDIA_ID}~n", []),
    io:format("To set an account's media file as MOH, enter: /{ACCOUNT_ID}/{MEDIA_ID}~n", []),
    io:format("To set an third-party HTTP url, enter: http://other.server.com/moh.mp3~n~n", []),
    number_builder_moh(NumberJObj);
metaflow_jobj(NumberJObj, MOH) ->
    kz_json:set_values([{<<"module">>, <<"hold">>}
                       ,{<<"data">>, moh_data(MOH)}
                       ], NumberJObj).

-spec moh_data(string()) -> kz_json:object().
moh_data("n") ->
    kz_json:new();
moh_data(MOH) ->
    kz_json:from_list([{<<"moh">>, kz_term:to_binary(MOH)}]).

%%------------------------------------------------------------------------------
%%% @doc From the kapps_call, create an event proplist that mimics what
%%% Ecallmanager would send after receiving a hold event from the Freeswitch
%%% node handling a call when a standard SIP hold was initiated.
%%% @end
%%------------------------------------------------------------------------------
-spec generate_event(kz_term:ne_binary(), kapps_call:call()) -> kz_term:proplist().
generate_event(EventName, Call) ->
    CallID = kapps_call:call_id(Call),
    Direction = kapps_call:direction(Call),
    SwitchNodename = kapps_call:switch_nodename(Call),
    CCVs = kapps_call:custom_channel_vars(Call),

    props:filter_empty([{<<"Event-Name">>, EventName}
                       ,{<<"Call-Direction">>, Direction}
                       ,{<<"Call-ID">>, CallID}
                       ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
                       ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
                       ,{<<"Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
                       ,{<<"Callee-ID-Number">>, kapps_call:callee_id_number(Call)}
                       ,{<<"Channel-Call-State">>, call_state(EventName)}
                       ,{<<"Channel-State">>, <<"EXECUTE">>}
                       ,{<<"Disposition">>, <<"SUCCESS">>}
                       ,{<<"From-Tag">>, kapps_call:from_tag(Call)}
                       ,{<<"Media-Server">>, SwitchNodename}
                       ,{<<"Msg-ID">>, erlang:system_time('microsecond')}
                       ,{<<"Other-Leg-Call-ID">>, kapps_call:other_leg_call_id(Call)}
                       ,{<<"Other-Leg-Direction">>, other_leg_direction(Direction)}
                       ,{<<"Presence-ID">>, kz_json:get_ne_binary_value(<<"Presence-ID">>, CCVs)}
                       ,{<<"Switch-Hostname">>, kapps_call:switch_hostname(Call)}
                       ,{<<"Switch-Nodename">>, SwitchNodename}
                       ,{<<"Switch-URI">>, kapps_call:switch_uri(Call)}
                       ,{<<"Switch-URL">>, kapps_call:switch_url(Call)}
                       ,{<<"Timestamp">>, kz_time:now_s()}
                       ,{<<"To-Tag">>, kapps_call:to_tag(Call)}
                       ,{<<"Custom-SIP-Headers">>, kapps_call:custom_sip_headers(Call)}
                       ,{<<"Custom-Channel-Vars">>, CCVs}
                       ,{<<"Custom-Application-Vars">>, kapps_call:custom_application_vars(Call)}
                        | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                       ]).

-spec call_state(kz_term:ne_binary()) -> kz_term:ne_binary().
call_state(<<"CHANNEL_HOLD">>) -> <<"HELD">>;
call_state(<<"CHANNEL_UNHOLD">>) -> <<"UNHELD">>.

-spec other_leg_direction(kz_term:ne_binary()) -> kz_term:ne_binary().
other_leg_direction(<<"outbound">>) -> <<"inbound">>;
other_leg_direction(_) -> <<"outbound">>.
