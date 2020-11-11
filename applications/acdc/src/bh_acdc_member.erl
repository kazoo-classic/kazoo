%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, Kage DS Ltd
%%% @doc
%%% @author Alan R Evans
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_acdc_member).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include_lib("../blackhole/src/blackhole.hrl").

-define(BINDING(),
       [
        <<"acdc.member.call.*.*">>
       ,<<"acdc.member.call_result.#">>
    ]).

-spec init() -> any().
init() ->
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.acdc_member_call">>, ?MODULE, 'validate'),
    _ = blackhole_bindings:bind(<<"blackhole.events.bindings.acdc_member_call">>, ?MODULE, 'bindings'),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.acdc_member_call_result">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.acdc_member_call_result">>, ?MODULE, 'bindings').

init_bindings() ->
    Bindings = ?BINDING(),
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"member">>], Bindings) of
        {'ok', _} -> lager:debug("initialized ACDC member bindings");
        {'error', _E} -> lager:info("failed to initialize ACDC member bindings: ~p", [_E])
    end.

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{key := <<"acdc_member_call">>
                   }) ->
    Context;
validate(Context, #{key := <<"acdc_member_call_result">>
                   }) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for ACDC member subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,key := <<"acdc_member_call">>
                    }=Map) ->
    Requested = <<"acdc.member.call">>,
    Subscribed = [<<"acdc.member.call.", AccountId/binary, ".*">>],
    Listeners = [{'amqp', 'acdc_queue', bind_options(AccountId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,key := <<"acdc_member_call_result">>
                    }=Map) ->
    Requested = <<"acdc.member.call_result">>,
    Subscribed = [<<"acdc.member.call_result.", AccountId/binary, ".#">>],
    Listeners = [{'amqp', 'acdc_queue', bind_options(AccountId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

-spec bind_options(kz_term:ne_binary()) -> kz_term:proplist().
bind_options(AccountId) ->
    [
     {'account_id', AccountId}
    ,'federate'
    ].
