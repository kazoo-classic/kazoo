-define(KNM_THQ_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".thinq">>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG_WRITE(Format, Args), ?debugFmt(Format, Args)).
-define(DEBUG_APPEND(Format, Args), ?debugFmt(Format, Args)).
-else.
-define(THQ_DEBUG, kapps_config:get_is_true(?KNM_THQ_CONFIG_CAT, <<"debug">>, 'false')).
-define(THQ_DEBUG_FILE, "/tmp/thinq.xml").
-define(DEBUG_WRITE(Format, Args),
        _ = ?THQ_DEBUG
        andalso file:write_file(?THQ_DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?THQ_DEBUG
        andalso file:write_file(?THQ_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).
-endif.

-define(THQ_BASE_URL, "https://api.thinq.com").

-ifdef(TEST).
-define(THQ_ACCOUNT_ID, "eunit_testing_account").
-else.
-define(THQ_ACCOUNT_ID
       ,kapps_config:get_string(?KNM_THQ_CONFIG_CAT, <<"account_id">>, "")
       ).
-endif.

-define(IS_SANDBOX_PROVISIONING_TRUE
       ,kapps_config:get_is_true(?KNM_THQ_CONFIG_CAT, <<"sandbox_provisioning">>, 'true')
       ).
-define(IS_PROVISIONING_ENABLED
       ,kapps_config:get_is_true(?KNM_THQ_CONFIG_CAT, <<"enable_provisioning">>, 'true')
       ).
-define(THQ_ORDER_NAME_PREFIX
       ,kapps_config:get_string(?KNM_THQ_CONFIG_CAT, <<"order_name_prefix">>, "Kazoo")
       ).

-define(THQ_API_USERNAME
       ,kapps_config:get_binary(?KNM_THQ_CONFIG_CAT, <<"api_username">>, <<>>)
       ).
-define(THQ_API_PASSWORD
       ,kapps_config:get_binary(?KNM_THQ_CONFIG_CAT, <<"api_password">>, <<>>)
       ).
-define(THQ_SIP_PEER
       ,kapps_config:get_string(?KNM_THQ_CONFIG_CAT, <<"sip_peer">>, "")
       ).
-define(THQ_SITE_ID
       ,kapps_config:get_string(?KNM_THQ_CONFIG_CAT, <<"site_id">>, "")
       ).

-define(MAX_SEARCH_QUANTITY
       ,kapps_config:get_pos_integer(?KNM_THQ_CONFIG_CAT, <<"max_search_quantity">>, 500)
       ).