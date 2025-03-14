%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Execute node commands
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_node_command).

-export([handle_req/2]).

-include("ecallmgr.hrl").

-define(NODE_CMD_CONFIG, <<"node_commands">>).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Node = props:get_value('node', Props),
    Options = props:get_value('node_options', Props),
    'true' = kapi_switch:fs_command_v(JObj),
    Cmd = kz_json:get_ne_binary_value(<<"Command">>, JObj),
    Args = kz_json:get_value(<<"Args">>, JObj),
    exec_cmd(Cmd, Args, JObj, Node, Options).

-spec exec_cmd(kz_term:ne_binary(), kz_term:api_object(), kz_json:object(), atom(), kz_term:proplist()) -> 'ok'.
exec_cmd(<<"send_http">>, 'undefined', JObj, _Node, _Options) ->
    lager:debug("received http_send command with empty arguments"),
    reply_error(<<"no arguments">>, JObj);
exec_cmd(<<"send_http">>, Args, JObj, Node, Options) ->
    Version = props:get_value('client_version', Options),
    lager:debug("received http_send command for node ~s with version ~s", [Node, Version]),
    Url = kz_json:get_ne_binary_value(<<"Url">>, Args),
    File = kz_json:get_value(<<"File-Name">>, Args),
    HttpFun = case Version >= <<"mod_kazoo v1.4">> of
                  'true' -> <<"kz_http_">>;
                  'false' -> <<"http_">>
              end,
    Method = <<HttpFun/binary, (kz_json:get_value(<<"Http-Method">>, Args, <<"put">>))/binary>>,
    Default = kapps_config:is_true(?APP_NAME, [?NODE_CMD_CONFIG, <<"send_http">>, <<"delete_on_success">>], 'false'),
    DeleteOnSuccess = kz_json:is_true(<<"Delete-On-Success">>, JObj, Default),
    send_http(Node, Version, File, Url, Method, JObj, DeleteOnSuccess);

exec_cmd(Cmd, _Args, JObj, _Node, _Options) ->
    reply_error(<<Cmd/binary, " not_implemented">>, JObj).

-spec reply_error(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
reply_error(Error, JObj) ->
    Values = [{<<"Result">>, <<"error">>}
             ,{<<"Error">>, Error}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kz_amqp_worker:cast(API, fun(P) -> kapi_switch:publish_reply(Queue, P) end).

-spec reply_error(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> 'ok'.
reply_error(Error, EventData, JObj) ->
    Values = [{<<"Result">>, <<"error">>}
             ,{<<"Error">>, Error}
             ,{<<"Event-Data">>, EventData}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kz_amqp_worker:cast(API, fun(P) -> kapi_switch:publish_reply(Queue, P) end).

-spec reply_success(kz_json:object()) -> 'ok'.
reply_success(JObj) ->
    reply_success(JObj, []).

-spec reply_success(kz_json:object(), kz_term:proplist()) -> 'ok'.
reply_success(JObj, Response) ->
    Values = [{<<"Result">>, <<"success">>}
             ,{<<"Response">>, kz_json:from_list(Response)}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    API = kz_json:set_values(Values, kz_api:remove_defaults(JObj)),
    Queue = kz_api:server_id(JObj),
    kz_amqp_worker:cast(API, fun(P) -> kapi_switch:publish_reply(Queue, P) end).

-spec send_http(atom(), binary(), binary(), binary(), kz_term:ne_binary(), kz_json:object(), boolean()) -> 'ok'.
send_http(Node, Version, File, Url, Method, JObj, DeleteOnSuccess) ->
    lager:debug("processing http_send command : ~s / ~s", [File, Url]),
    Args = <<Url/binary, " ", File/binary>>,
    {Api, Fun} = send_http_api_and_callback_funs(Version),
    M = kz_term:to_atom(Method, 'true'),
    A = kz_term:to_list(Args),
    case Api(Node, M, A, Fun, [JObj, DeleteOnSuccess, File, Node]) of
        {'error', _} -> reply_error(<<"failure">>, JObj);
        {'ok', JobId} -> lager:debug("send_http command started ~p", [JobId])
    end.

send_http_api_and_callback_funs(Version)->
    case Version >= <<"mod_kazoo v1.4">> of
        'true' -> {fun freeswitch:bgapi4/5, fun send_http_cb/4};
        'false' -> {fun freeswitch:bgapi/5, fun send_http_cb/3}
    end.

-spec send_http_cb(atom(), kz_term:ne_binary(), list()) -> 'ok'.
send_http_cb('ok', <<"+OK", _/binary>>, [JobId, JObj, DeleteOnSuccess, File, Node]) ->
    lager:debug("processed http_send command with success : ~s", [JobId]),
    _ = maybe_delete_file(Node, File, DeleteOnSuccess),
    reply_success(JObj);
send_http_cb(_, Reply, [JobId, JObj | _]) ->
    lager:debug("error processing http_send : ~p : ~s", [Reply, JobId]),
    reply_error(Reply, JObj).

-spec send_http_cb(atom(), kz_term:ne_binary(), kz_term:proplist(), list()) -> 'ok'.
send_http_cb('ok', <<"+OK", _/binary>>, _FSProps, [JobId, JObj, DeleteOnSuccess, File, Node]) ->
    lager:debug("processed http_send command with success : ~s", [JobId]),
    _ = maybe_delete_file(Node, File, DeleteOnSuccess),
    reply_success(JObj);
send_http_cb(_, <<"-ERR ", Reply/binary>>, [_ | FSProps], [JobId, JObj | _]) ->
    lager:debug("error processing http_send : ~p : ~s", [Reply, JobId]),
    Props = ecallmgr_util:unserialize_fs_props(FSProps),
    reply_error(Reply, kz_json:from_list(Props), JObj);
send_http_cb(_, Reply, [_ | FSProps], [JobId, JObj | _]) ->
    lager:debug("error processing http_send : ~p : ~s", [Reply, JobId]),
    Props = ecallmgr_util:unserialize_fs_props(FSProps),
    reply_error(Reply, kz_json:from_list(Props), JObj).

-spec maybe_delete_file(atom(), binary(), boolean()) -> any().
maybe_delete_file(_Node, _File, 'false') -> 'ok';
maybe_delete_file(Node, File, 'true') ->
    freeswitch:api(Node, 'system', <<"rm ", File/binary>>).
