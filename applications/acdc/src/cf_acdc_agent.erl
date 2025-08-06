%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Handles changing an agent's status
%%% "data":{
%%%   "action":["login","logout","paused","resume", "toggle", "toggle_paused"] // one of these
%%%   ,"timeout":600 // in seconds, for "paused" status
%%%   ,"presence_id":"abc123" // id of the button
%%%   ,"presence_prefix_agent":"{prefix e.g. *555}" // prefix for presence event, will add agent ID as suffix to this
%%%   ,"presence_state":["early", "confirmed","terminated"
%%%                      ,"red_flash", "red_solid", "green"
%%%                     ]
%%% }
%%%
%%% Setting "presence_prefix_agent" allows this callflow module to be used for
%%% a BLF key feature that will toggle and show an agent's status with a button.
%%% Set the presence_prefix_agent value to the prefix code you want to use
%%% i.e. "*555". Then, instead of setting an extension number on the callflow,
%%% set a pattern with the code you want to use for a prefix followed by the
%%% user ID: e.g. "^\*555(.{32})$". On the phone, set the BLF key to monitor
%%% and dial "*555{this user's ID}".
%%%
%%% @author James Aimonetti
%%% @author Ruel Tmeizeh (RuhNet https://ruhnet.co)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_agent).

-export([handle/2
        ,find_agent/1
        ,find_agent_status/2
        ,play_not_an_agent/1
        ,play_agent_invalid/1
        ,login_agent/2
        ,logout_agent/2
        ]).

-include("acdc_config.hrl").
-include_lib("callflow/src/callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    kapps_call_command:answer(Call),
    _ = case find_agent(Call) of
            {'ok', 'undefined'} ->
                lager:info("no owner on this device == no agent"),
                play_not_an_agent(Call);
            {'ok', AgentId} ->
                Status = find_agent_status(Call, AgentId),
                NewStatus = fix_data_status(kz_json:get_value(<<"action">>, Data)),
                lager:info("agent ~s maybe changing status from ~s to ~s", [AgentId, Status, NewStatus]),

                maybe_update_status(Call, AgentId, Status, NewStatus, Data);
            {'error', 'multiple_owners'} ->
                lager:info("too many owners of device ~s, not logging in", [kapps_call:authorizing_id(Call)]),
                play_agent_invalid(Call)
        end,
    lager:info("finished with acdc agent callflow"),
    cf_exe:continue(Call).

%%------------------------------------------------------------------------------
%% @doc Get a normalized current agent status value.
%% @end
%%------------------------------------------------------------------------------
-spec find_agent_status(kapps_call:call() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
find_agent_status(?NE_BINARY = AccountId, AgentId) ->
    fix_agent_status(acdc_agent_util:most_recent_status(AccountId, AgentId));
find_agent_status(Call, AgentId) ->
    find_agent_status(kapps_call:account_id(Call), AgentId).

%%------------------------------------------------------------------------------
%% @doc Normalizes agent status values.
%% @end
%%------------------------------------------------------------------------------
-spec fix_agent_status({'ok', kz_term:ne_binary()}) -> kz_term:ne_binary().
fix_agent_status({'ok', <<"resume">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"wrapup">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"busy">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"logout">>}) -> <<"logged_out">>;
fix_agent_status({'ok', <<"login">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"outbound">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"answered">>}) -> <<"ready">>;
fix_agent_status({'ok', <<"ringing">>}) -> <<"ready">>;
fix_agent_status({'ok', Status}) -> Status.

fix_data_status(<<"pause">>) -> <<"paused">>;
fix_data_status(Status) -> Status.

%%------------------------------------------------------------------------------
%% @doc Update an agent's status if the action is valid for the current status.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_status(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_update_status(Call, AgentId, Status, <<"toggle">>, Data) ->
    toggle_agent(Call, AgentId, Status, Data);
maybe_update_status(Call, AgentId, Status, <<"toggle_paused">>, Data) ->
    toggle_agent_paused(Call, AgentId, Status, Data);
maybe_update_status(Call, AgentId, _Curr, <<"logout">>, Data) ->
    lager:info("agent ~s wants to log out (currently: ~s)", [AgentId, _Curr]),
    logout_agent(Call, AgentId, Data),
    play_agent_logged_out(Call);
maybe_update_status(Call, AgentId, <<"logged_out">>, <<"resume">>, _Data) ->
    lager:debug("agent ~s is logged out, resuming doesn't make sense", [AgentId]),
    play_agent_invalid(Call);
maybe_update_status(Call, AgentId, <<"logged_out">>, <<"login">>, Data) ->
    maybe_login_agent(Call, AgentId, Data);
maybe_update_status(Call, AgentId, <<"unknown">>, <<"login">>, Data) ->
    maybe_login_agent(Call, AgentId, Data);
maybe_update_status(Call, AgentId, <<"ready">>, <<"login">>, Data) ->
    lager:info("agent ~s is already logged in", [AgentId]),
    _ = play_agent_logged_in_already(Call),
    send_new_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_login/1, 'undefined');
maybe_update_status(Call, AgentId, FromStatus, <<"paused">>, Data) ->
    maybe_pause_agent(Call, AgentId, FromStatus, Data);
maybe_update_status(Call, AgentId, <<"paused">>, <<"ready">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"paused">>, <<"resume">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"outbound">>, <<"resume">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, AgentId, <<"ready">>, <<"resume">>, Data) ->
    lager:info("agent ~s is coming back from pause", [AgentId]),
    resume_agent(Call, AgentId, Data),
    play_agent_resume(Call);
maybe_update_status(Call, _AgentId, _Status, _NewStatus, _Data) ->
    lager:info("agent ~s: invalid status change from ~s to ~s", [_AgentId, _Status, _NewStatus]),
    play_agent_invalid(Call).

%%------------------------------------------------------------------------------
%% @doc Toggle an agent's status. If an agent is paused, they will be unpaused,
%% rather than being logged out.
%% @end
%%------------------------------------------------------------------------------
-spec toggle_agent(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
toggle_agent(Call, AgentId, <<"ready">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"red">>, Data),
    logout_agent(Call, AgentId, Data2);
toggle_agent(Call, AgentId, <<"wrapup">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"red">>, Data),
    logout_agent(Call, AgentId, Data2);
toggle_agent(Call, AgentId, <<"outbound">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"red">>, Data),
    logout_agent(Call, AgentId, Data2);
toggle_agent(Call, AgentId, <<"paused">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"green">>, Data),
    resume_agent(Call, AgentId, Data2);
toggle_agent(Call, AgentId, <<"logged_out">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"green">>, Data),
    maybe_login_agent(Call, AgentId, Data2);
toggle_agent(Call, AgentId, <<"unknown">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"green">>, Data),
    maybe_login_agent(Call, AgentId, Data2).

%%------------------------------------------------------------------------------
%% @doc Toggle an agent's ready/paused status. BLF flashes red when paused.
%% @end
%%------------------------------------------------------------------------------
-spec toggle_agent_paused(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
toggle_agent_paused(Call, AgentId, <<"ready">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"red_flash">>, Data),
    pause_agent(Call, AgentId, Data2);
toggle_agent_paused(Call, AgentId, <<"wrapup">>, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"red_flash">>, Data),
    pause_agent(Call, AgentId, Data2);
toggle_agent_paused(Call, AgentId, Status, Data) ->
    Data2 = kz_json:set_value(<<"presence_state">>, <<"green">>, Data),
    maybe_resume_agent(Call, AgentId, Status, Data2).

-spec maybe_login_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_login_agent(Call, AgentId, Data) ->
    lager:debug("agent ~s wants to log in", [AgentId]),
    case login_agent(Call, AgentId, Data) of
        <<"success">> -> play_agent_logged_in(Call);
        <<"failed">> -> play_agent_invalid(Call)
    end.

maybe_pause_agent(Call, AgentId, <<"ready">>, Data) ->
    Timeout = kapps_call:kvs_fetch('cf_capture_group', Call),
    lager:info("agent pause time: ~p", [Timeout]),
    case Timeout of
        undefined -> pause_agent(Call, AgentId, Data);
        T -> pause_agent(Call, AgentId, Data, binary_to_integer(T) * 60)
    end;
maybe_pause_agent(Call, _AgentId, FromStatus, _Data) ->
    lager:info("unable to go from ~s to paused", [FromStatus]),
    play_agent_invalid(Call).

%%------------------------------------------------------------------------------
%% @doc Resume an agent if the action is valid for the current status.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_resume_agent(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
maybe_resume_agent(Call, AgentId, Status, Data) ->
    case lists:member(Status, [<<"paused">>, <<"outbound">>, <<"ready">>, <<"wrapup">>]) of
        'true' ->
            resume_agent(Call, AgentId, Data);
        'false' ->
            lager:info("agent ~s cannot resume when status is ~s", [AgentId, Status]),
            play_agent_invalid(Call)
    end.

-spec login_agent(kapps_call:call(), kz_term:ne_binary()) -> api_kz_term:ne_binary().
login_agent(Call, AgentId) ->
    login_agent(Call, AgentId, kz_json:new()).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `login' message.
%% @end
%%------------------------------------------------------------------------------
-spec login_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_ne_binary().
login_agent(Call, AgentId, Data) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, kapps_call:account_id(Call)}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Presence-ID">>, presence_id(Data, AgentId)}
               ,{<<"Presence-State">>, presence_state(Data)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    case kz_amqp_worker:call(Update
                            ,fun kapi_acdc_agent:publish_login/1
                            ,fun kapi_acdc_agent:login_resp_v/1
                            )
    of
        {'ok', RespJObj} ->
            lager:info("agent ~s is logging in", [AgentId]),
            kz_json:get_value(<<"Status">>, RespJObj);
        {'error', _E} ->
            lager:debug("failed to hear back about login: ~p", [_E]),
            <<"failed">>
    end.

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `logout' message.
%% @end
%%------------------------------------------------------------------------------
-spec logout_agent(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
logout_agent(Call, AgentId) ->
    logout_agent(Call, AgentId, kz_json:new()).

-spec logout_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
logout_agent(Call, AgentId, Data) ->
    lager:info("agent ~s is logging out", [AgentId]),
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_logout/1).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `pause' message.
%% @end
%%------------------------------------------------------------------------------
-spec pause_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_term:api_integer()) ->
          kapps_call:kapps_api_std_return().
pause_agent(Call, AgentId, Data, Timeout) ->
    _ = play_agent_pause(Call),
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_pause/1, Timeout).

-spec pause_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
pause_agent(Call, AgentId, Data) ->
    Timeout = kz_json:get_integer_value(<<"timeout">>
                                       ,Data
                                       ,kapps_config:get_integer(<<"acdc">>, <<"default_agent_pause_timeout">>, 600)
                                       ),
    lager:info("agent ~s is pausing work for ~b s", [AgentId, Timeout]),
    pause_agent(Call, AgentId, Data, Timeout).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent `resume' message.
%% @end
%%------------------------------------------------------------------------------
-spec resume_agent(kapps_call:call(), kz_term:ne_binary(), kz_json:object()) ->
          kapps_call:kapps_api_std_return().
resume_agent(Call, AgentId, Data) ->
    update_agent_status(Call, AgentId, Data, fun kapi_acdc_agent:publish_resume/1).

-spec update_agent_status(kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_amqp_worker:publish_fun()) -> 'ok'.
update_agent_status(Call, AgentId, Data, PubFun) ->
    update_agent_status(Call, AgentId, Data, PubFun, 'undefined').
update_agent_status(Call, AgentId, Data, PubFun, Timeout) ->
    send_new_status(Call, AgentId, Data, PubFun, Timeout).

%%------------------------------------------------------------------------------
%% @doc Publish an AMQP agent status-change message.
%% @end
%%------------------------------------------------------------------------------
-spec send_new_status(kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_amqp_worker:publish_fun(), kz_term:api_integer() | kz_term:ne_binary()) -> 'ok'.
send_new_status(Call, AgentId, Data, PubFun, Timeout) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, kapps_call:account_id(Call)}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Time-Limit">>, Timeout}
               ,{<<"Presence-ID">>, presence_id(Data, AgentId)}
               ,{<<"Presence-State">>, presence_state(Data)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    PubFun(Update).

-spec presence_id(kz_json:object(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
presence_id(Data, AgentId) ->
    PresId = kz_json:get_ne_binary_value(<<"presence_id">>, Data),
    Prefix = kz_json:get_ne_binary_value(<<"presence_prefix_agent">>, Data), % overrides presence_id
    case {PresId, Prefix} of
        {'undefined', 'undefined'} -> 'undefined'; %% nothing specified
        {PresId, 'undefined'} -> PresId;           %% only presence_id specified, so use it
        _ -> <<Prefix/binary, AgentId/binary>>     %% prefix is specified (used for callflow BLF e.g. *555{userId})
    end.

-spec presence_state(kz_json:object()) -> kz_term:api_ne_binary().
presence_state(Data) ->
    format_presence_state(kz_json:get_ne_binary_value(<<"presence_state">>, Data)).

format_presence_state(<<"green">>) -> <<"terminated">>;
format_presence_state(<<"terminated">> = T) -> T;
format_presence_state(<<"red_flash">>) -> <<"early">>;
format_presence_state(<<"early">> = E) -> E;
format_presence_state(<<"red_solid">>) -> <<"confirmed">>;
format_presence_state(<<"confirmed">> = C) -> C;
format_presence_state(_) -> 'undefined'.

-type find_agent_error() :: 'unknown_endpoint' | 'multiple_owners'.
-spec find_agent(kapps_call:call()) ->
          {'ok', kz_term:api_binary()} |
          {'error', find_agent_error()}.
find_agent(Call) ->
    find_agent(Call, kapps_call:authorizing_id(Call)).

find_agent(_Call, 'undefined') ->
    {'error', 'unknown_endpoint'};
find_agent(Call, EndpointId) ->
    {'ok', Endpoint} = kz_datamgr:open_doc(kapps_call:account_db(Call), EndpointId),
    find_agent(Call, Endpoint, kz_json:get_value([<<"hotdesk">>, <<"users">>], Endpoint)).

find_agent(Call, Endpoint, 'undefined') ->
    find_agent_owner(Call, kz_json:get_value(<<"owner_id">>, Endpoint));
find_agent(Call, Endpoint, Owners) ->
    case kz_json:get_keys(Owners) of
        [] -> find_agent_owner(Call, kz_json:get_value(<<"owner_id">>, Endpoint));
        [OwnerId] -> {'ok', OwnerId};
        _ -> {'error', 'multiple_owners'}
    end.

find_agent_owner(Call, 'undefined') -> {'ok', kapps_call:owner_id(Call)};
find_agent_owner(_Call, EPOwnerId) -> {'ok', EPOwnerId}.

-spec play_not_an_agent(kapps_call:call()) -> kapps_call:kapps_api_std_return().
play_not_an_agent(Call) -> kapps_call_command:b_prompt(<<"agent-not_call_center_agent">>, Call).
play_agent_logged_in_already(Call) -> kapps_call_command:b_prompt(<<"agent-already_logged_in">>, Call).
play_agent_logged_in(Call) -> kapps_call_command:b_prompt(<<"agent-logged_in">>, Call).
play_agent_logged_out(Call) -> kapps_call_command:b_prompt(<<"agent-logged_out">>, Call).
play_agent_resume(Call) -> kapps_call_command:b_prompt(<<"agent-resume">>, Call).
play_agent_pause(Call) -> kapps_call_command:b_prompt(<<"agent-pause">>, Call).

-spec play_agent_invalid(kapps_call:call()) -> kapps_call:kapps_api_std_return().
play_agent_invalid(Call) -> kapps_call_command:b_prompt(<<"agent-invalid_choice">>, Call).
