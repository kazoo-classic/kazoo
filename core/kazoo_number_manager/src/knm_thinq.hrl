-define(KNM_THQ_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".thinq">>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG_WRITE(Format, Args), ?debugFmt(Format, Args)).
-define(DEBUG_APPEND(Format, Args), ?debugFmt(Format, Args)).
-else.
-define(THQ_DEBUG, kapps_config:get_is_true(?KNM_THQ_CONFIG_CAT, <<"debug">>, 'false')).
-define(THQ_DEBUG_FILE, "/tmp/thinq.log").
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

-define(CNAM_ID, <<"cnam_id">>).
-define(ADDRESS_ID, <<"address_id">>).


-define(DEFAULT_THQ_ACCOUNT_ID, "").
-define(THQ_ACCOUNT_ID(AccountId, ResellerId)
       ,kz_term:to_list(kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"account_id">>, ?THQ_ACCOUNT_ID(ResellerId)))
       ).
-define(THQ_ACCOUNT_ID(AccountId)
       ,kz_term:to_list(kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"account_id">>, ?THQ_ACCOUNT_ID))
       ).
-ifdef(TEST).
-define(THQ_ACCOUNT_ID, "eunit_testing_account").
-else.
-define(THQ_ACCOUNT_ID
       ,kapps_config:get_string(?KNM_THQ_CONFIG_CAT, <<"account_id">>, ?DEFAULT_THQ_ACCOUNT_ID)
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
-define(THQ_IS_UPSTREAM_INBOUND_CNAM_ENABLED
       ,kapps_config:get_is_true(?KNM_THQ_CONFIG_CAT, <<"enable_upstream_inbound_cnam">>, 'false')
       ).

-define(DEFAULT_THQ_API_USERNAME, <<>>).
-define(THQ_API_USERNAME(AccountId, ResellerId)
       ,kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"api_username">>, ?THQ_API_USERNAME(ResellerId))
       ).
-define(THQ_API_USERNAME(AccountId)
       ,kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"api_username">>, ?THQ_API_USERNAME)
       ).
-define(THQ_API_USERNAME
       ,kapps_config:get_binary(?KNM_THQ_CONFIG_CAT, <<"api_username">>, ?DEFAULT_THQ_API_USERNAME)
       ).

-define(DEFAULT_THQ_API_PASSWORD, <<>>).
-define(THQ_API_PASSWORD(AccountId, ResellerId)
       ,kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"api_password">>, ?THQ_API_PASSWORD(ResellerId))
       ).
-define(THQ_API_PASSWORD(AccountId)
       ,kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"api_password">>, ?THQ_API_PASSWORD)
       ).
-define(THQ_API_PASSWORD
       ,kapps_config:get_binary(?KNM_THQ_CONFIG_CAT, <<"api_password">>, ?DEFAULT_THQ_API_PASSWORD)
       ).

-define(THQ_SIP_PEER
       ,kapps_config:get_string(?KNM_THQ_CONFIG_CAT, <<"sip_peer">>, "")
       ).

-define(DEFAULT_THQ_SITE_ID, <<>>).
-define(THQ_SITE_ID(AccountId, ResellerId)
       ,kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"site_id">>, ?THQ_SITE_ID(ResellerId))
       ).
-define(THQ_SITE_ID(AccountId)
       ,kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"site_id">>, ?THQ_SITE_ID)
       ).
-define(THQ_SITE_ID
       ,kapps_config:get_binary(?KNM_THQ_CONFIG_CAT, <<"site_id">>, ?DEFAULT_THQ_SITE_ID)
       ).

-define(MAX_SEARCH_QUANTITY
       ,kapps_config:get_pos_integer(?KNM_THQ_CONFIG_CAT, <<"max_search_quantity">>, 500)
       ).

-define(DEFAULT_THQ_ENABLE_SMS, 'false').
-define(THQ_ENABLE_SMS(AccountId, ResellerId)
       ,kz_term:to_atom(kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"enable_sms">>, ?THQ_ENABLE_SMS(ResellerId)))
       ).
-define(THQ_ENABLE_SMS(AccountId)
       ,kz_term:to_atom(kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"enable_sms">>, ?THQ_ENABLE_SMS))
       ).
-define(THQ_ENABLE_SMS
       ,kapps_config:get_atom(?KNM_THQ_CONFIG_CAT, <<"enable_sms">>, ?DEFAULT_THQ_ENABLE_SMS)
       ).

-define(DEFAULT_THQ_TF_ENABLE_SMS, 'false').
-define(THQ_TF_ENABLE_SMS(AccountId, ResellerId)
       ,kz_term:to_atom(kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"tf_enable_sms">>, ?THQ_TF_ENABLE_SMS(ResellerId)))
       ).
-define(THQ_TF_ENABLE_SMS(AccountId)
       ,kz_term:to_atom(kapps_account_config:get_ne_binary(AccountId, ?KNM_THQ_CONFIG_CAT, <<"tf_enable_sms">>, ?THQ_TF_ENABLE_SMS))
       ).
-define(THQ_TF_ENABLE_SMS
       ,kapps_config:get_atom(?KNM_THQ_CONFIG_CAT, <<"tf_enable_sms">>, ?DEFAULT_THQ_TF_ENABLE_SMS)
       ).
