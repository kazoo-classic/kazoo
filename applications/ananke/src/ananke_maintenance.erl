-module(ananke_maintenance).

-export([load_schedule/2
         ,reset/0]).

-spec load_schedule(kz_term:ne_binary(), kz_json:object()) -> normal.
load_schedule(Name, JObj) ->
    ananke_listener:load_schedule(Name, JObj).

-spec reset() -> 'ok'.
reset() ->
    'ok' = ananke_tasks_sup:reset(),
    [ amqp_cron:cancel_task(Name)  || {Name, _, _, _} <- amqp_cron:task_list()],
    'ok'.
