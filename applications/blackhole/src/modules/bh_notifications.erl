%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2022, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Roman Galeev
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_notifications).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").
-include_lib("kazoo_documents/include/doc_types.hrl").

-define(NOTIFICATION(System, Action)
       ,<<"notifications.", System/binary, ".", Action/binary>>
       ).
-define(SYSTEMS, [<<"voicemail">>,<<"sip">>]).
-define(ACTIONS, [<<"register">>, <<"new">>, <<"saved">>]).

-spec init() -> any().
init() ->
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.notification">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.notification">>, ?MODULE, 'bindings').

init_bindings() ->
    Bindings = [<<"notification.*.*">>],
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"notification">>], Bindings) of
        {'ok', _} -> lager:debug("initialized notification bindings");
        {'error', _E} -> lager:info("failed to initialize notification bindings: ~p", [_E])
    end.

%% example binding: object.fax.doc_update

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"*">>, <<"*">>]}) ->
    Context;
validate(Context, #{keys := [System, <<"*">>]}) ->
    case lists:member(System, ?SYSTEMS) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"system ", System/binary, ".* not supported">>)
    end;
validate(Context, #{keys := [System, Action]
                   }) ->
    case lists:member(System, ?SYSTEMS)
        andalso lists:member(Action, ?ACTIONS)
    of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"binding notification.", System/binary, ".", Action/binary, " not supported">>)
    end;
validate(Context, #{keys := [System, Action, _AccountId]
                   }) ->
    case lists:member(System, ?SYSTEMS)
        andalso lists:member(Action, ?ACTIONS)
    of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"binding notification.", System/binary, ".", Action/binary, " not supported">>)
    end;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for object subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [System, Action]
                    }=Map) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Requested = ?NOTIFICATION(System, Action),
    Map#{requested => Requested
        ,subscribed => subscribed(System, Action, AccountDb)
        ,listeners => listeners(System, Action, AccountDb)
        }.

subscribed(System, Action, _AccountDb) ->
    [?NOTIFICATION(System, Action)].

listeners(_, _Action, _AccountDb) ->
    [{'amqp', 'notifications', [{'restrict_to'
    ,[kapi_definition:restrict_to(Definition)
      || Definition <- get_notifications_definition()
     ]
    },
    'federate'
   ]
}
].

-spec get_notifications_definition() -> kapi_definition:apis().
get_notifications_definition() ->
    [Definition
     || Definition <- kapi_notifications:api_definitions(),
        kapi_definition:name(Definition) =/= <<"skel">>,
        kapi_definition:name(Definition) =/= <<"notify_update">>
    ].
