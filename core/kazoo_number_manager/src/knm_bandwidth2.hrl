-define(KNM_BW2_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".bandwidth2">>).

-define(DEFAULT_BW2_ACCOUNT_ID, "").
-define(BW2_ACCOUNT_ID(AccountId, ResellerId)
       ,kz_term:to_list(kapps_account_config:get_ne_binary(AccountId, ?KNM_BW2_CONFIG_CAT, <<"account_id">>, ?BW2_ACCOUNT_ID(ResellerId)))
       ).
-define(BW2_ACCOUNT_ID(AccountId)
       ,kz_term:to_list(kapps_account_config:get_ne_binary(AccountId, ?KNM_BW2_CONFIG_CAT, <<"account_id">>, ?BW2_ACCOUNT_ID))
       ).
-ifdef(TEST).
-define(BW2_ACCOUNT_ID, "eunit_testing_account").
-else.
-define(BW2_ACCOUNT_ID
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"account_id">>, ?DEFAULT_BW2_ACCOUNT_ID)
       ).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG_WRITE(Format, Args), ?debugFmt(Format, Args)).
-define(DEBUG_APPEND(Format, Args), ?debugFmt(Format, Args)).
-else.
-define(BW2_DEBUG, kapps_config:get_is_true(?KNM_BW2_CONFIG_CAT, <<"debug">>, 'false')).
-define(BW2_DEBUG_FILE, "/tmp/bandwidth2.log").
-define(DEBUG_WRITE(Format, Args),
        _ = ?BW2_DEBUG
        andalso file:write_file(?BW2_DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?BW2_DEBUG
        andalso file:write_file(?BW2_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).
-endif.

-define(BW2_BASE_URL, "https://api.inetwork.com/v1.0").

-ifdef(TEST).
-define(ACCOUNT_ID, "eunit_testing_account").
-else.
-define(ACCOUNT_ID(Options)
        ,?BW2_ACCOUNT_ID(knm_carriers:account_id(Options), knm_carriers:reseller_id(Options))
        ).
-endif.

-define(IS_SANDBOX_PROVISIONING_TRUE
       ,kapps_config:get_is_true(?KNM_BW2_CONFIG_CAT, <<"sandbox_provisioning">>, 'true')
       ).
-define(IS_PROVISIONING_ENABLED
       ,kapps_config:get_is_true(?KNM_BW2_CONFIG_CAT, <<"enable_provisioning">>, 'true')
       ).
-define(BW2_ORDER_NAME_PREFIX
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"order_name_prefix">>, "Kazoo")
       ).

-define(BW2_API_USERNAME
       ,kapps_config:get_binary(?KNM_BW2_CONFIG_CAT, <<"api_username">>, <<>>)
       ).
-define(BW2_API_PASSWORD
       ,kapps_config:get_binary(?KNM_BW2_CONFIG_CAT, <<"api_password">>, <<>>)
       ).
-define(BW2_SIP_PEER
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"sip_peer">>, "")
       ).
-define(BW2_SITE_ID
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"site_id">>, "")
       ).

-define(MAX_SEARCH_QUANTITY
       ,kapps_config:get_pos_integer(?KNM_BW2_CONFIG_CAT, <<"max_search_quantity">>, 500)
       ).

-define(BW2_ORDER_POLL_INTERVAL, 2000).

-define(ORDER_NUMBER_XPATH, "ExistingTelephoneNumberOrderType/TelephoneNumberList/TelephoneNumber/text()").
-define(CUSTOMER_ORDER_ID_XPATH, "CustomerOrderId/text()").
-define(ORDER_NAME_XPATH, "Name/text()").
-define(BW_ORDER_ID_XPATH, "Order/id/text()").
-define(BW_ORDER_STATUS_XPATH, "OrderStatus/text()").
-define(BW_DISC_ORDER_ID_XPATH, "orderRequest/id/text()").