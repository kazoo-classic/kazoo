-ifndef(ANANKE_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"ananke">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-record(callback, {callback_number       :: kz_term:api_binary()
                  ,is_callback_disabled  :: boolean()
                  ,call_timeout          :: pos_integer()
                  ,schedule              :: pos_integers()
                  }).

-record(args, {account_id            :: kz_term:api_binary()
              ,user_id               :: kz_term:api_binary()
              ,vm_box_id             :: kz_term:api_binary()
              ,vm_number             :: kz_term:api_binary()
              ,realm                 :: kz_term:api_binary()
              ,callbacks             :: callbacks()
              }).

-type pos_integers() :: list(pos_integer()).
-type check_fun() :: 'true' | fun(() -> boolean()) | {Module :: atom(), FunName :: atom(), Args :: list()}.

-type args() :: #args{}.
-type callback() :: #callback{}.
-type callbacks() :: list(callback()).

-define(ANANKE_HRL, 'true').
-endif.
