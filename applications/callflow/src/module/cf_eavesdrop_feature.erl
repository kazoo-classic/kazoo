%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Eavesdrop feature code
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`group_id'</dt>
%%%   <dd>Group ID.</dd>
%%%
%%%   <dt>`approved_device_id'</dt>
%%%   <dd>Device ID.</dd>
%%%
%%%   <dt>`approved_user_id'</dt>
%%%   <dd>User ID.</dd>
%%%
%%%   <dt>`approved_group_id'</dt>
%%%   <dd>Group ID.</dd>
%%% </dl>
%%%
%%% `group_id' defines a list of eavesdrop targets. If `group_id' is
%%% `undefined' then anybody can be eavesdropped.
%%% One of the `approved_device_id', `approved_user_id' or `approved_group_id'
%%% must be defined to access feature code.
%%%
%%%
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_eavesdrop_feature).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2
        ,get_target_for_extension/2
        ]).

-export_type([target/0]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    Exten = kapps_call:kvs_fetch('cf_capture_group', Call),
    Target = get_target_for_extension(Exten, Call),
    Table = fields_to_check(),

    case cf_util:check_value_of_fields(Table, 'false', Data, Call)
        andalso Target /= 'error'
    of
        'true' ->
            NewData = maybe_correct_target(Target, kz_json:get_ne_binary_value(<<"group_id">>, Data), Call),
            Flow = kz_json:from_list([{<<"data">>, build_flow_data(Call, NewData)}
                                     ,{<<"module">>, <<"eavesdrop">>}
                                     ,{<<"children">>, kz_json:new()}
                                     ]),
            cf_exe:branch(Flow, Call);
        'false' ->
            _ = cf_eavesdrop:no_permission_to_eavesdrop(Call),
            cf_exe:stop(Call)
    end.

-spec fields_to_check() -> kz_term:proplist().
fields_to_check() ->
    [{<<"approved_device_id">>, fun(Id, Call) -> Id == kapps_call:authorizing_id(Call) end}
    ,{<<"approved_user_id">>, fun cf_util:caller_belongs_to_user/2}
    ,{<<"approved_group_id">>, fun cf_util:caller_belongs_to_group/2}
    ].

-type target() :: {'ok', kz_term:proplist()} | 'error'.

-spec build_flow_data(kapps_call:call(), kz_term:proplist()) -> kz_json:object().
build_flow_data(Call, Data) ->
    build_flow_data(Call, Data, kapps_call:authorizing_type(Call)).

-spec build_flow_data(kapps_call:call(), kz_term:proplist(), kz_term:api_binary()) -> kz_json:object().
build_flow_data(Call, Data, AuthorizingType)
  when AuthorizingType =:= <<"device">>;
       AuthorizingType =:= <<"mobile">> ->
    kz_json:from_list([{<<"approved_device_id">>, kapps_call:authorizing_id(Call)}
                       | Data
                      ]);
build_flow_data(Call, Data, <<"user">>) ->
    kz_json:from_list([{<<"approved_user_id">>, kapps_call:authorizing_id(Call)}
                       | Data
                      ]);
build_flow_data(_Call, Data, _AuthorizingType) ->
    lager:debug("unhandled authorizing type ~s", [_AuthorizingType]),
    kz_json:from_list(Data).

-spec get_target_for_extension(kz_term:ne_binary(), kapps_call:call()) ->
                                      target().
get_target_for_extension(Exten, Call) ->
    case cf_flow:lookup(Exten, kapps_call:account_id(Call)) of
        {'ok', Callflow, _} ->
            lookup_endpoint(kz_json:get_value(<<"flow">>, Callflow));
        {'error', _} -> 'error'
    end.

-spec maybe_correct_target(target(), kz_term:api_binary(), kapps_call:call()) ->
                                  kz_term:proplist().
maybe_correct_target({'ok', Data}, 'undefined', _Call) ->
    Data;
maybe_correct_target({'ok', Data}, GroupId, Call) ->
    GroupMembers = find_group_members(GroupId, Call),
    DeviceIds = props:get_value(<<"device_ids">>, Data, []),
    UserIds = props:get_value(<<"user_ids">>, Data, []),
    FilteredDeviceIds = lists:filter(fun(DeviceId) -> lists:member(DeviceId, GroupMembers) end, DeviceIds),
    FilteredUserIds = lists:filter(fun(UserId) -> lists:member(UserId, GroupMembers) end, UserIds),
    [{<<"device_ids">>, FilteredDeviceIds}
    ,{<<"user_ids">>, FilteredUserIds}].

-spec find_group_members(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binaries().
find_group_members(GroupId, Call) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), GroupId) of
        {'error', _E} -> [];
        {'ok', GroupJObj} ->
            kz_json:get_keys(<<"endpoints">>, GroupJObj)
    end.

-spec lookup_endpoint(kz_term:api_object()) -> target().
lookup_endpoint('undefined') -> 'error';
lookup_endpoint(Flow) ->
    lookup_endpoint(Flow, kz_json:get_ne_binary_value(<<"module">>, Flow)).

-spec lookup_endpoint(kz_json:object(), kz_term:api_binary()) -> target().
lookup_endpoint(Flow, <<"device">>) ->
    DeviceId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], Flow),
    Data = [{<<"device_ids">>, [DeviceId]}],
    {'ok', Data};
lookup_endpoint(Flow, <<"user">>) ->
    UserId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], Flow),
    Data = [{<<"user_ids">>, [UserId]}],
    {'ok', Data};
lookup_endpoint(Flow, <<"ring_group">>) ->
    Endpoints = kz_json:get_list_value([<<"data">>, <<"endpoints">>], Flow),
    DeviceIds = lists:filtermap(fun(JObj) -> get_targed_id(<<"device">>, JObj) end, Endpoints),
    UserIds = lists:filtermap(fun(JObj) -> get_targed_id(<<"user">>, JObj) end, Endpoints),
    Data = [{<<"device_ids">>, DeviceIds}
           ,{<<"user_ids">>, UserIds}],
    {'ok', Data};
lookup_endpoint(Flow, _TargetType) ->
    Child = kz_json:get_json_value([<<"children">>, ?DEFAULT_CHILD_KEY], Flow),
    lookup_endpoint(Child).

-spec get_targed_id(kz_term:api_binary(), kz_json:object()) -> boolean() | {'true', kz_term:api_binary()}.
get_targed_id(TargetType, JObj) ->
    case kz_json:get_binary_value(<<"endpoint_type">>, JObj) of
        TargetType -> {'true', kz_json:get_binary_value(<<"id">>, JObj)};
        _ -> 'false'
    end.
