%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2025, KageDS
%%% @doc inter account dialing
%%%
%%% @author Alan R Evans
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_inter_account).
-behaviour(gen_cf_action).

-include("callflow.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"action">>, Data, <<"call">>) of
        <<"call">> ->
            handle_call(Data, Call, kapps_call:kvs_fetch(<<"cf_capture_groups">>, Call));
        _ ->
            cf_exe:continue(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
handle_call(_Data, Call, 'undefined') ->
    lager:error("No capture group found"),
    cf_exe:continue(Call);
handle_call(Data, Call, JObj) ->
    Number = kz_json:get_ne_binary_value(<<"digits">>, JObj),
    AccountId = kz_json:get_ne_binary_value(
                    kz_json:get_ne_binary_value(<<"office_id">>, JObj)
                    ,kz_json:get_value(<<"office_id">>, Data)),
    CallerIdPrefix = kz_json:get_ne_binary_value(<<"caller_id_prefix">>, Data, <<"">>),
    case cf_flow:lookup(Number, AccountId) of
        {'ok', Flow, NoMatch} ->
            lager:info("callflow ~s in account ~s satisfies request for ~s", [kz_doc:id(Flow), AccountId, Number]),
            Updates = [{fun kapps_call:set_request/2, list_to_binary([Number, "@", kapps_call:request_realm(Call)])}
                      ,{fun kapps_call:set_to/2, list_to_binary([Number, "@", kapps_call:to_realm(Call)])}
                      ,{fun kapps_call:set_account_id/2, AccountId}
                      ,{fun kapps_call:set_caller_id_number/2, <<CallerIdPrefix/binary, Number/binary>>}
                      ],
            cf_exe:set_call(kapps_call:exec(Updates, Call)),
            cf_exe:branch(kz_json:get_json_value(<<"flow">>, Flow), Call);
        _ ->
            lager:info("failed to find a callflow to satisfy ~s", [Number]),
            cf_exe:continue(Call)
    end.
