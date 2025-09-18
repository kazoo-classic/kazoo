%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2025, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Ruel Tmeizeh
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_util).

-export([listen_on_other_leg/2]).
-export([send_hangup_req/1]).
-export([send_break_req/1]).
-export([moh/2]).

-include("konami.hrl").

-spec listen_on_other_leg(kapps_call:call(), kz_term:ne_binaries()) -> 'ok'.
listen_on_other_leg(Call, Events) ->
    API = [{<<"Application-Name">>, <<"noop">>}
          ,{<<"B-Leg-Events">>, Events}
          ,{<<"Insert-At">>, <<"now">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("sending noop for b leg events"),
    kapps_call_command:send_command(API, Call).

-spec send_hangup_req(kz_term:ne_binary()) -> 'ok'.
send_hangup_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
          ,{<<"Action">>, <<"hangup">>}
          ,{<<"Data">>, kz_json:new()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to hangup ~s", [CallId]),
    kz_amqp_worker:cast(API, fun kapi_metaflow:publish_action/1).

-spec send_break_req(kz_term:ne_binary()) -> 'ok'.
send_break_req(CallId) ->
    API = [{<<"Call-ID">>, CallId}
          ,{<<"Action">>, <<"break">>}
          ,{<<"Data">>, kz_json:new()}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("attempting to break ~s", [CallId]),
    kz_amqp_worker:cast(API, fun kapi_metaflow:publish_action/1).

-spec moh(kz_term:api_binary(), kapps_call:call()) -> kz_term:ne_binary().
moh(MOH, Call) ->
    %% It may be possible (and more efficient) to use <<"Hold-Media">>
    %% in the CCVs in the kapps_call, but this approach guarantees
    %% correct selection of MoH:
    AccountID = kapps_call:account_id(Call),

    {'ok', AccountDoc} = kzd_accounts:fetch(AccountID),
    AccountMOH = kzd_accounts:music_on_hold_media_id(AccountDoc),

    {'ok', UserDoc} = kzd_users:fetch(AccountID, kapps_call:owner_id(Call)),
    AccountOrUserMOH = kzd_users:music_on_hold_media_id(UserDoc, AccountMOH),

    DefaultMOH = case AccountOrUserMOH of
                     'undefined' -> ?SILENCE;
                     <<"silen",_/binary>> -> ?SILENCE;
                     MediaId -> kz_media_util:media_path(MediaId, AccountID)
                 end,
    case MOH of
        'undefined' -> DefaultMOH;
        <<"silen", _/binary>> -> ?SILENCE; % matches silence/silent as well as silence_stream://x
        <<"pulse">> -> <<"tone_stream://v=-25%(2000,0,432,428.8);loops=-1">>;
        <<"beep">> -> ?DEFAULT_BEEP;
        <<"beep", _Freq/binary>> -> <<"tone_stream://%(100,15000,", _Freq/binary, ");loops=-1">>; % configurable beep
        <<"tone_stream://", _/binary>>=_Tones -> _Tones; % allow arbitrary tones to be specified
        <<"shout://", _/binary>>=_MP3Stream -> _MP3Stream;
        <<"local_stream://", _/binary>>=_LocalStream -> _LocalStream;
        _MediaId -> kz_media_util:media_path(MOH, AccountID)
    end.
