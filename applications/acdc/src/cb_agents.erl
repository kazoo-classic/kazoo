%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% CRUD for call queues
%%% /agents
%%%   GET: list all known agents and their queues
%%%
%%% /agents/stats
%%%   GET: call stats for all agents for the last hour
%%% /agents/stats_summary
%%%   GET: aggregate call stats for all agents
%%% /agents/status
%%%   GET: current status for all agents
%%% /agents/{agent_id}
%%%   GET: agent details
%%% /agents/{agent_id}}/stats
%%%   GET: call stats for  agent_id 
%%% /agents/{agent_id}}/stats_summary
%%%   GET: aggregate call stats for agent_id
%%% /agents/{agent_id}/queue_status
%%%   POST: login/logout agent to/from queue
%%%
%%% /agents/{agent_id}/restart
%%%   POST: force-restart a stuck agent
%%%
%%% /agents/{agent_id}/status
%%%   GET: current status for agent_id
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(cb_agents).

-export([init/0
        ,authorize/3
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,content_types_provided/1, content_types_provided/2, content_types_provided/3
        ,validate/1, validate/2, validate/3
        ,post/3
        ]).

-include_lib("crossbar/src/crossbar.hrl").
-include("acdc_config.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".queues">>).

-define(FORMAT_COMPRESSED, <<"compressed">>).
-define(FORMAT_VERBOSE, <<"verbose">>).

-define(CB_LIST, <<"agents/crossbar_listing">>).
-define(STATS_PATH_TOKEN, <<"stats">>).
-define(STATS_SUMMARY_PATH_TOKEN, <<"stats_summary">>).
-define(STATUS_PATH_TOKEN, <<"status">>).
-define(QUEUE_STATUS_PATH_TOKEN, <<"queue_status">>).
-define(RESTART_PATH_TOKEN, <<"restart">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kapi_acdc_agent:declare_exchanges(),
    _ = kapi_acdc_stats:declare_exchanges(),

    _ = crossbar_bindings:bind(<<"*.allowed_methods.agents">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.agents">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.agents">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.execute.post.agents">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.validate.agents">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authorize.agents">>, ?MODULE, 'authorize').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _, ?RESTART_PATH_TOKEN) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> 'true';
        'false' ->
            Context1 = cb_context:add_system_error('forbidden', Context),
            {'halt', Context1}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() -> [?HTTP_GET].

allowed_methods(?STATUS_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(?STATS_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(?STATS_SUMMARY_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(_UserId) -> [?HTTP_GET].

allowed_methods(?STATUS_PATH_TOKEN, _UserId) -> [?HTTP_GET, ?HTTP_POST];
allowed_methods(_UserId, ?STATUS_PATH_TOKEN) -> [?HTTP_GET, ?HTTP_POST];
allowed_methods(?STATS_PATH_TOKEN, _UserId) -> [?HTTP_GET];
allowed_methods(_UserId, ?STATS_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(?STATS_SUMMARY_PATH_TOKEN, _UserId) -> [?HTTP_GET];
allowed_methods(_UserId, ?STATS_SUMMARY_PATH_TOKEN) -> [?HTTP_GET];
allowed_methods(_UserId, ?QUEUE_STATUS_PATH_TOKEN) -> [?HTTP_GET, ?HTTP_POST];
allowed_methods(_UserId, ?RESTART_PATH_TOKEN) -> [?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /agents => []
%%    /agents/foo => [<<"foo">>]
%%    /agents/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?STATUS_PATH_TOKEN) -> 'true';
resource_exists(?STATUS_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?STATS_PATH_TOKEN) -> 'true';
resource_exists(?STATS_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?STATS_SUMMARY_PATH_TOKEN) -> 'true';
resource_exists(?STATS_SUMMARY_PATH_TOKEN, _) -> 'true';
resource_exists(_, ?QUEUE_STATUS_PATH_TOKEN) -> 'true';
resource_exists(_, ?RESTART_PATH_TOKEN) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token()) -> cb_context:context().
content_types_provided(Context) -> Context.
content_types_provided(Context, ?STATUS_PATH_TOKEN) -> Context;
content_types_provided(Context, ?STATS_PATH_TOKEN) ->
    case cb_context:req_value(Context, <<"format">>, ?FORMAT_COMPRESSED) of
        ?FORMAT_VERBOSE ->
            cb_context:add_content_types_provided(Context
                                                 ,[{'to_json', ?JSON_CONTENT_TYPES}
                                                  ,{'to_csv', ?CSV_CONTENT_TYPES}
                                                  ]);
        ?FORMAT_COMPRESSED ->
            cb_context:add_content_types_provided(Context
                                                 ,[{'to_json', ?JSON_CONTENT_TYPES}]
                                                 )
    end.
content_types_provided(Context, ?STATUS_PATH_TOKEN, _) -> Context;
content_types_provided(Context, _, ?STATUS_PATH_TOKEN) -> Context;
content_types_provided(Context, ?STATS_SUMMARY_PATH_TOKEN, _) -> Context;
content_types_provided(Context, _, ?STATS_SUMMARY_PATH_TOKEN) -> Context;
content_types_provided(Context, _, ?QUEUE_STATUS_PATH_TOKEN) -> Context;
content_types_provided(Context, _, ?RESTART_PATH_TOKEN) -> Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /agents mights load a list of agent objects
%% /agents/123 might load the agent object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->
                      cb_context:context().
validate(Context) ->
    summary(Context).

validate(Context, PathToken) ->
    validate_agent(Context, PathToken, cb_context:req_verb(Context)).

validate_agent(Context, ?STATUS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_all_agent_statuses(Context);
validate_agent(Context, ?STATS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_stats('all', Context, 'false');
validate_agent(Context, ?STATS_SUMMARY_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_stats('all', Context, 'true');
validate_agent(Context, Id, ?HTTP_GET) ->
    read(Id, Context).

validate(Context, AgentId, PathToken) ->
    validate_agent_action(Context, AgentId, PathToken, cb_context:req_verb(Context)).

validate_agent_action(Context, AgentId, ?STATUS_PATH_TOKEN, ?HTTP_POST) ->
    validate_status_change(read(AgentId, Context));
validate_agent_action(Context, AgentId, ?STATUS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_status(AgentId, Context);
validate_agent_action(Context, ?STATUS_PATH_TOKEN, AgentId, ?HTTP_GET) ->
    fetch_agent_status(AgentId, Context);
validate_agent_action(Context, AgentId, ?STATS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_stats(AgentId, Context, 'false');
validate_agent_action(Context, ?STATS_PATH_TOKEN, AgentId, ?HTTP_GET) ->
    fetch_agent_stats(AgentId, Context, 'false');
validate_agent_action(Context, AgentId, ?STATS_SUMMARY_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_stats(AgentId, Context, 'true');
validate_agent_action(Context, ?STATS_SUMMARY_PATH_TOKEN, AgentId, ?HTTP_GET) ->
    fetch_agent_stats(AgentId, Context, 'true');
validate_agent_action(Context, AgentId, ?QUEUE_STATUS_PATH_TOKEN, ?HTTP_POST) ->
    OnSuccess = fun (C) -> maybe_queues_change(read(AgentId, C)) end,
    cb_context:validate_request_data(<<"queue_update">>, Context, OnSuccess);
validate_agent_action(Context, AgentId, ?QUEUE_STATUS_PATH_TOKEN, ?HTTP_GET) ->
    fetch_agent_queues(read(AgentId, Context));
validate_agent_action(Context, AgentId, ?RESTART_PATH_TOKEN, ?HTTP_POST) ->
    read(AgentId, Context).

-spec maybe_queues_change(cb_context:context()) -> cb_context:context().
maybe_queues_change(Context) ->
    case cb_context:resp_status(Context) of
        'success' -> handle_queue_update(Context);
        _ -> Context
    end.

-spec handle_queue_update(cb_context:context()) -> cb_context:context().
handle_queue_update(Context) ->
    QueueID = cb_context:req_value(Context, <<"queue_id">>),
    Updater = case cb_context:req_value(Context, <<"action">>) of
                  <<"login">> -> fun(Doc) -> kzd_agent:maybe_add_queue(Doc, QueueID) end;
                  <<"logout">> -> fun(Doc) -> kzd_agent:maybe_rm_queue(Doc, QueueID) end
              end,
    cb_context:update_doc(Context, Updater).

-spec fetch_agent_queues(cb_context:context()) -> cb_context:context().
fetch_agent_queues(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            Doc = cb_context:doc(Context),
            Queues = kz_json:get_value(<<"queues">>, Doc),
            cb_context:set_resp_data(Context, Queues);
        _ ->
            Context
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, AgentId, ?STATUS_PATH_TOKEN) ->
    case cb_context:req_value(Context, <<"status">>) of
        <<"login">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_login/1);
        <<"logout">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_logout/1);
        <<"pause">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_pause/1);
        <<"resume">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_resume/1);
        <<"end_wrapup">> -> publish_update(Context, AgentId, fun kapi_acdc_agent:publish_end_wrapup/1)
    end,
    crossbar_util:response(<<"status update sent">>, Context);
post(Context, AgentId, ?QUEUE_STATUS_PATH_TOKEN) ->
    publish_action(Context, AgentId),
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            Queues = kz_json:get_value(<<"queues">>, cb_context:doc(Context), []),
            cb_context:set_resp_data(Context1, Queues);
        _Status ->
            Context1
    end;
post(Context, AgentId, ?RESTART_PATH_TOKEN) ->
    publish_restart(Context, AgentId),
    crossbar_util:response(kz_json:new(), Context).

-spec publish_action(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
publish_action(Context, AgentId) ->
    Publisher = case cb_context:req_value(Context, <<"action">>) of
                    <<"logout">> -> fun kapi_acdc_agent:publish_logout_queue/1;
                    <<"login">> -> fun kapi_acdc_agent:publish_login_queue/1
                end,

    Props = props:filter_undefined(
              [{<<"Account-ID">>, cb_context:account_id(Context)}
              ,{<<"Agent-ID">>, AgentId}
              ,{<<"Queue-ID">>, cb_context:req_value(Context, <<"queue_id">>)}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ]),

    kz_amqp_worker:cast(Props, Publisher).

-spec publish_update(cb_context:context(), kz_term:api_binary(), function()) -> 'ok'.
publish_update(Context, AgentId, PubFun) ->
    Update = props:filter_undefined(
               [{<<"Account-ID">>, cb_context:account_id(Context)}
               ,{<<"Agent-ID">>, AgentId}
               ,{<<"Time-Limit">>, cb_context:req_value(Context, <<"timeout">>)}
               ,{<<"Alias">>, cb_context:req_value(Context, <<"alias">>)}
               ,{<<"Presence-ID">>, cb_context:req_value(Context, <<"presence_id">>)}
               ,{<<"Presence-State">>, cb_context:req_value(Context, <<"presence_state">>)}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    kz_amqp_worker:cast(Update, PubFun).

-spec publish_restart(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
publish_restart(Context, AgentId) ->
    Payload = [{<<"Account-ID">>, cb_context:account_id(Context)}
              ,{<<"Agent-ID">>, AgentId}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, fun kapi_acdc_agent:publish_restart/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(path_token(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_user:type())).

-define(CB_AGENTS_LIST, <<"users/crossbar_listing">>).
-spec fetch_all_agent_statuses(cb_context:context()) -> cb_context:context().
fetch_all_agent_statuses(Context) ->
    case kz_term:is_true(cb_context:req_value(Context, <<"recent">>)) of
        'false' ->
            fetch_current_status(Context
                                 ,'undefined'
                                 ,cb_context:req_value(Context, <<"status">>) 
                                );
        'true' ->
            fetch_all_statuses(Context
                               ,'undefined'
                               ,cb_context:req_value(Context, <<"status">>)
                              )
    end.

-spec fetch_agent_status(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
fetch_agent_status(AgentId, Context) ->
    case kz_term:is_true(cb_context:req_value(Context, <<"recent">>)) of
        'false' ->
            fetch_current_status(Context
                                 ,AgentId
                                 ,cb_context:req_value(Context, <<"status">>)
                                 );
        'true' ->
            fetch_all_statuses(Context
                               ,AgentId
                               ,cb_context:req_value(Context, <<"status">>)
                              )
    end.

-spec fetch_agent_stats(kz_term:api_binary(), cb_context:context(), boolean()) -> cb_context:context().
fetch_agent_stats(AgentId, Context, Summarize) ->
    case cb_context:req_value(Context, <<"start_range">>) of
        'undefined' -> fetch_current_agent_stats(AgentId, Context, Summarize);
        StartRange -> fetch_ranged_agent_stats(AgentId, Context, StartRange, Summarize)
    end.

-spec fetch_stats_summary_from_amqp(cb_context:context(), kz_term:kz_proplist()) -> cb_context:context().
fetch_stats_summary_from_amqp(Context, Req) ->
    case kz_amqp_worker:call(Req
                                     ,fun kapi_acdc_stats:publish_agent_calls_req/1
                                     ,fun kapi_acdc_stats:agent_calls_resp_v/1
                                     )
    of
        {'error', Resp} -> format_stats_summary_error(Context, Resp);
        {'ok', Resp} -> format_stats_summary_response(Context, Resp)
    end.

-spec format_stats_summary_response(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_stats_summary_response(Context, Resp) ->
    case kz_json:get_value(<<"Event-Name">>, Resp) of
        <<"agent_calls_err">> -> format_stats_summary_error(Context, Resp);
        <<"agent_calls_resp">> ->  format_stats_summary(Context, Resp)
    end.

-spec format_stats_summary_error(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_stats_summary_error(Context, Resp) ->
    crossbar_util:response('error', <<"stat request had errors">>, 400
                            ,kz_json:get_value(<<"Error-Reason">>, Resp)
                            ,Context
    ).

-spec fetch_current_agent_stats(kz_term:api_binary() | 'all', cb_context:context() , boolean()) -> cb_context:context().
fetch_current_agent_stats(AgentId, Context, Summarize) ->
    Now = kz_time:current_tstamp(),
    Yday = Now - ?SECONDS_IN_DAY,

    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Agent-ID">>, case AgentId of
                                        'all' -> cb_context:req_value(Context, <<"agent_id">>);
                                        Else -> Else
                              end}
            ,{<<"Start-Range">>, Yday}
            ,{<<"End-Range">>, Now}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_stats_from_amqp(Context, Req, Summarize).

-spec fetch_current_status(cb_context:context(), kz_term:api_binary(),  kz_term:api_binary()) -> cb_context:context().
fetch_current_status(Context, AgentId, Status) ->
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Status">>, Status}
             ,{<<"Agent-ID">>, AgentId}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case kz_amqp_worker:call(Req
                            ,fun kapi_acdc_stats:publish_agent_cur_status_req/1
                            ,fun kapi_acdc_stats:agent_cur_status_resp_v/1
                            )
    of
        {'error', Resp} -> format_current_status_error(Context, Resp);
        {'ok', Resp} -> format_current_status_response(Context, Resp)
    end.

-spec format_current_status_response(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_current_status_response(Context, Resp) ->
    case kz_json:get_value(<<"Event-Name">>, Resp) of
        <<"agent_cur_status_err">> -> format_current_status_error(Context, Resp);
        <<"agent_cur_status_resp">> ->  format_current_status_stats(Context, Resp)
    end.

-spec format_current_status_error(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_current_status_error(Context, Resp) ->
    crossbar_util:response('error', <<"stat request had errors">>, 400
                            ,kz_json:get_value(<<"Error-Reason">>, Resp)
                            ,Context
    ).

-spec format_current_status_stats(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_current_status_stats(Context, Resp) ->
    crossbar_util:response(kz_json:get_value(<<"Agents">>, Resp, kz_json:new()), Context).

-spec fetch_all_statuses(cb_context:context(), kz_term:api_binary(), kz_term:api_binary()) ->
                                        cb_context:context().
fetch_all_statuses(Context, AgentId, Status) ->
    Now = kz_time:now_s(),
    From = Now - min(?SECONDS_IN_DAY, ?ACDC_CLEANUP_WINDOW),

    Opts = props:filter_undefined(
             [{<<"Status">>, Status}
             ,{<<"Agent-ID">>, AgentId}
             ,{<<"Start-Range">>, From}
             ,{<<"End-Range">>, Now}
             ,{<<"Limit">>, cb_context:req_value(Context, <<"limit">>)}
             ]),

    {'ok', Resp} = acdc_agent_util:most_recent_statuses(cb_context:account_id(Context), Opts),
    crossbar_util:response(Resp, Context).

-spec fetch_ranged_agent_stats(kz_term:api_binary(), cb_context:context(), pos_integer(), boolean()) -> cb_context:context().
fetch_ranged_agent_stats(AgentId, Context, StartRange, Summarize) ->
    MaxRange = ?ACDC_CLEANUP_WINDOW,

    Now = kz_time:current_tstamp(),
    Past = Now - MaxRange,

    To = kz_term:to_integer(cb_context:req_value(Context, <<"end_range">>, Now)),
    MaxFrom = To - MaxRange,

    case kz_term:to_integer(StartRange) of
        F when F > To ->
            %% start_range is larger than end_range
            Msg = kz_json:from_list([{<<"message">>, <<"value is greater than start_range">>}
                                    ,{<<"cause">>, To}
                                    ]),
            cb_context:add_validation_error(<<"end_range">>, <<"maximum">>, Msg, Context);
        F when F < MaxFrom ->
            %% Range is too big
            fetch_ranged_agent_stats(AgentId, Context, MaxFrom, To, MaxFrom >= Past, Summarize);
        F when F < Past, To > Past ->
            %% range overlaps archived/real data, use real
            fetch_ranged_agent_stats(AgentId, Context, Past, To, Past >= Past, Summarize);
        F ->
            fetch_ranged_agent_stats(AgentId, Context, F, To, F >= Past, Summarize)
    end.

-spec fetch_ranged_agent_stats(kz_term:api_binary(), cb_context:context(), pos_integer(), pos_integer(), boolean(), boolean) ->
                                      cb_context:context().
fetch_ranged_agent_stats(AgentId, Context, From, To, 'true', Summarize) ->
    lager:debug("ranged query from ~b to ~b(~b) of current stats (now ~b)", [From, To, To-From, kz_time:current_tstamp()]),
    Req = props:filter_undefined(
            [{<<"Account-ID">>, cb_context:account_id(Context)}
            ,{<<"Status">>, cb_context:req_value(Context, <<"status">>)}
            ,{<<"Agent-ID">>, case AgentId of
                                        'all' -> cb_context:req_value(Context, <<"agent_id">>);
                                        Else -> Else
                              end}
            ,{<<"Start-Range">>, From}
            ,{<<"End-Range">>, To}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    fetch_stats_from_amqp(Context, Req, Summarize);
fetch_ranged_agent_stats(_AgentId, Context, From, To, 'false', _Summarize) ->
    lager:debug("ranged query from ~b to ~b of archived stats", [From, To]),
    Context.

-spec fetch_stats_from_amqp(cb_context:context(), kz_term:kz_proplist(), boolean()) -> cb_context:context().
fetch_stats_from_amqp(Context, Req, 'true') ->
    fetch_stats_summary_from_amqp(Context, Req);
fetch_stats_from_amqp(Context, Req, Summarize) ->
    case kz_amqp_worker:call(Req
                            ,fun kapi_acdc_stats:publish_current_calls_req/1
                            ,fun kapi_acdc_stats:current_calls_resp_v/1
                            )
    of
        {'error', Resp} -> format_error(Context, Resp);
        {'ok', Resp} -> format_stats(Context, Resp)
    end.

-spec format_error(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_error(Context, Resp) ->
    crossbar_util:response('error', <<"stat request had errors">>, 400
                            ,kz_json:get_value(<<"Error-Reason">>, Resp)
                            ,Context
    ).


-spec format_stats_summary(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_stats_summary(Context, Resp) ->
    Stats = kz_json:get_value(<<"Handled">>, Resp, [])
    ++ kz_json:get_value(<<"Abandoned">>, Resp, [])
    ++ kz_json:get_value(<<"Waiting">>, Resp, [])
    ++ kz_json:get_value(<<"Processed">>, Resp, [])
    ++ kz_json:get_value(<<"Missed">>, Resp, []),
    crossbar_util:response(
            lists:foldl(fun format_stats_fold/2
                     ,kz_json:new()
                     ,Stats 
            )
            ,Context).    

-spec format_stats(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
format_stats(Context, Resp) ->
    Stats = kz_doc:public_fields(kz_json:get_value(<<"Handled">>, Resp, []))
        ++ kz_doc:public_fields(kz_json:get_value(<<"Abandoned">>, Resp, []))
        ++ kz_doc:public_fields(kz_json:get_value(<<"Waiting">>, Resp, []))
        ++ kz_doc:public_fields(kz_json:get_value(<<"Processed">>, Resp, [])),
    crossbar_util:response(Stats,Context).


-spec format_stats_fold(kz_json:object(), kz_json:object()) ->
                               kz_json:object().
format_stats_fold(Stat, Acc) ->
    QueueId = kz_json:get_value(<<"queue_id">>, Stat),

    case kz_json:get_value(<<"agent_id">>, Stat) of
        'undefined' -> Acc;
        AgentId ->
            TotalsK = [AgentId, <<"total_calls">>],
            QTotalsK = [AgentId, <<"queues">>, QueueId, <<"total_calls">>],

            Totals = kz_json:get_integer_value(TotalsK, Acc, 0),
            QTotals = kz_json:get_integer_value(QTotalsK, Acc, 0),

            AnsweredData = maybe_add_answered(Stat, Acc),
            MissedData = maybe_add_missed(Stat, Acc),
            kz_json:set_values([{TotalsK, Totals + 1}
                                ,{QTotalsK, QTotals + 1}]
                                ++ AnsweredData
                                ++ MissedData
                               ,Acc
                               )
    end.

-spec maybe_add_answered(kz_json:object(), kz_json:object()) ->
                                [{kz_json:path(), non_neg_integer()}].
-spec maybe_add_answered(kz_json:object(), kz_json:object(), kz_term:api_binary()) ->
                                [{kz_json:path(), non_neg_integer()}].
maybe_add_answered(Stat, Acc) ->
    maybe_add_answered(Stat, Acc, kz_json:get_value(<<"status">>, Stat)).
maybe_add_answered(Stat, Acc, <<"handled">>) ->
    add_answered(Stat, Acc);
maybe_add_answered(Stat, Acc, <<"processed">>) ->
    add_answered(Stat, Acc);
maybe_add_answered(_, _, _S) ->
    lager:debug("status ~s not indicative of an answered call", [_S]),
    [].

-spec add_answered(kz_json:object(), kz_json:object()) ->
                          [{kz_json:path(), non_neg_integer()},...].
add_answered(Stat, Acc) ->
    AgentId = kz_json:get_value(<<"agent_id">>, Stat),
    QueueId = kz_json:get_value(<<"queue_id">>, Stat),
    STalkTime = kz_json:get_value(<<"talk_time">>, Stat, 0),

    AnsweredK = [AgentId, <<"answered_calls">>],
    QAnsweredK = [AgentId, <<"queues">>, QueueId, <<"answered_calls">>],
 
    TalkTimeK = [AgentId, <<"talk_time">>],
    QTalkTimeK = [AgentId, <<"queues">>, QueueId, <<"talk_time">>],

    Answered = kz_json:get_integer_value(AnsweredK, Acc, 0),
    QAnswered = kz_json:get_integer_value(QAnsweredK, Acc, 0),

    TalkTime = kz_json:get_integer_value(TalkTimeK, Acc, 0),
    QTalkTime = kz_json:get_integer_value(QTalkTimeK, Acc, 0),

    [{AnsweredK, Answered + 1}
    ,{QAnsweredK, QAnswered + 1}
    ,{TalkTimeK, TalkTime + STalkTime}
    ,{QTalkTimeK, QTalkTime + STalkTime}
    ].

-spec maybe_add_missed(kz_json:object(), kz_json:object()) ->
                                [{kz_json:path(), non_neg_integer()}].
-spec maybe_add_missed(kz_json:object(), kz_json:object(), kz_term:api_binary()) ->
                                [{kz_json:path(), non_neg_integer()}].
maybe_add_missed(Stat, Acc) ->
    maybe_add_missed(Stat, Acc, kz_json:get_value(<<"status">>, Stat)).
maybe_add_missed(Stat, Acc, <<"missed">>) ->
    add_missed(Stat, Acc);
maybe_add_missed(_, _, _S) ->
    lager:debug("status ~s not indicative of an answered call", [_S]),
    [].

-spec add_missed(kz_json:object(), kz_json:object()) ->
                          [{kz_json:path(), non_neg_integer()},...].
add_missed(Stat, Acc) ->
    AgentId = kz_json:get_value(<<"agent_id">>, Stat),
    QueueId = kz_json:get_value(<<"queue_id">>, Stat),

    MissedK = [AgentId, <<"missed_calls">>],
    QMissedK = [AgentId, <<"queues">>, QueueId, <<"missed_calls">>],
 
    Missed = kz_json:get_integer_value(MissedK, Acc, 0),
    QMissed = kz_json:get_integer_value(QMissedK, Acc, 0),

    [{MissedK, Missed + 1}
    ,{QMissedK, QMissed + 1}
    ].






%% -spec add_misses_to_query(kz_json:object(), kz_json:object(), kz_term:ne_binary()) ->
%%                               kz_json:object().
%% add_misses_to_query(Context, Req0, Resp0) ->
%%     Req = props:set_value(<<"Status">>, <<"missed">>, Req0),
%%     case kz_amqp_worker:call(Req
%%                             ,fun kapi_acdc_stats:publish_agent_calls_req/1
%%                             ,fun kapi_acdc_stats:agent_calls_resp_v/1
%%                             )
%%     of
%%         {'error', Resp} -> format_error(Context, Resp);
%%         {'ok', Resp} -> format_stats_summary_response(Context, {Resp0, Resp})
%%     end.

%add_misses_to_resp(Data, Acc) ->
%    case kz_json:recursive_to_proplist(kz_json:get_value(<<"Missed">>, Data, [])) of
%        [] -> Acc;
%        Misses ->
%            lists:foldl(fun(Miss, AccJObj) ->
%                                add_miss(Miss, AccJObj)
%                        end
%                       ,Acc
%                       ,Misses
%                       )
%    end.
%
%-spec add_miss(list(), kz_json:object()) -> kz_json:object().
%add_miss(Miss, Acc) ->
%    AgentId = props:get_value(<<"agent_id">>, Miss),
%    QueueId = props:get_value(<<"queue_id">>, Miss),
%    MissesK = [AgentId, <<"missed_calls">>],
%    QMissesK = [AgentId, <<"queues">>, QueueId, <<"missed_calls">>],
%
%    Misses = kz_json:get_integer_value(MissesK, Acc, 0),
%    QMisses = kz_json:get_integer_value(QMissesK, Acc, 0),
%
%    TotalsK = [AgentId, <<"total_calls">>],
%    QTotalsK = [AgentId, <<"queues">>, QueueId, <<"total_calls">>],
%
%    Totals = kz_json:get_integer_value(TotalsK, Acc, 0),
%    QTotals = kz_json:get_integer_value(QTotalsK, Acc, 0),
%
%    kz_json:set_values([{MissesK, Misses + 1}
%                       ,{QMissesK, QMisses + 1}
%                       ,{TotalsK, Totals + 1}
%                       ,{QTotalsK, QTotals + 1}
%                       ]
%                      ,Acc
%                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) ->
                                    kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:set_value(<<"id">>
                      ,kz_doc:id(JObj)
                      ,kz_json:get_value(<<"value">>, JObj)
                      )
     | Acc
    ].

-spec validate_status_change(cb_context:context()) -> cb_context:context().
validate_status_change(Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            lager:debug("read agent doc"),
            validate_status_change(Context, cb_context:req_value(Context, <<"status">>));
        _ ->
            lager:debug("failed to read agent doc"),
            check_for_status_error(Context, cb_context:req_value(Context, <<"status">>))
    end.

-define(STATUS_CHANGES, [<<"login">>, <<"logout">>, <<"pause">>, <<"resume">>, <<"end_wrapup">>]).
-spec validate_status_change(cb_context:context(), kz_term:api_binary()) ->
                                    cb_context:context().
validate_status_change(Context, S) ->
    case lists:member(S, ?STATUS_CHANGES) of
        'true' -> validate_status_change_params(Context, S);
        'false' ->
            lager:debug("status ~s not valid", [S]),
            cb_context:add_validation_error(
              <<"status">>
                                           ,<<"enum">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"value is not a valid status">>}
                                              ,{<<"cause">>, S}
                                              ])
                                           ,Context
             )
    end.

-spec check_for_status_error(cb_context:context(), kz_term:api_binary()) ->
                                    cb_context:context().
check_for_status_error(Context, S) ->
    case lists:member(S, ?STATUS_CHANGES) of
        'true' -> Context;
        'false' ->
            lager:debug("status ~s not found", [S]),
            cb_context:add_validation_error(
              <<"status">>
                                           ,<<"enum">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"value is not a valid status">>}
                                              ,{<<"cause">>, S}
                                              ])
                                           ,Context
             )
    end.

-spec validate_status_change_params(cb_context:context(), kz_term:ne_binary()) ->
                                           cb_context:context().
validate_status_change_params(Context, <<"pause">>) ->
    Value = cb_context:req_value(Context, <<"timeout">>),
    try kz_term:to_integer(Value) of
        N when N >= 0 -> cb_context:set_resp_status(Context, 'success');
        N ->
            lager:debug("bad int for pause: ~p", [N]),
            cb_context:add_validation_error(
              <<"timeout">>
                                           ,<<"minimum">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"value must be at least greater than or equal to 0">>}
                                              ,{<<"cause">>, N}
                                              ])
                                           ,Context
             )
    catch
        _E:_R -> cb_context:set_resp_status(Context, 'success')
    end;
validate_status_change_params(Context, _S) ->
    lager:debug("great success for ~s", [_S]),
    cb_context:set_resp_status(Context, 'success').

