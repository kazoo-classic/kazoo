-ifndef(KONAMI_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"konami">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, ?APP_NAME).

-define(EVENT(CallId, EventName, Event)
       ,{'event', CallId, EventName, Event}
       ).

-define(WSD_ENABLED, kapps_config:get_is_true(?CONFIG_CAT, <<"webseq_enabled">>, 'false')).

-define(SILENCE, <<"silence_stream://-1">>).
-define(DEFAULT_BEEP, <<"tone_stream://%(100,15000,432);loops=-1">>). % 432Hz beep every 15 sec

-define(KONAMI_HRL, 'true').
-endif.
