%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2025, RuhNet
%%% @doc Handle the Number Attributes feature
%%% This Attributes feature can be used for setting some labels on a number, so
%%% that it can be treated differently than the default by internal or external
%%% systems/apps. The `group` and `class` parameters are arbitrary, the `options`
%%% parameter is a list of arbitrary strings, and the `traffic` parameter is
%%% used for different processing when this number is used as source or
%%% destination. When `traffic` is set to `fax`, calls to/from the number
%%% passing through Trunkstore will force media processing, add the `fax` flag
%%% to the call (if outbound), and enable T.38 variables.
%%% @author Ruel Tmeizeh (www.ruhnet.co)
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_attributes).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_ATTRIBUTES).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% update the attributes (if the number is in service).
%% @end
%%------------------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(Number) ->
    NumberState = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, NumberState).

-spec save(knm_number:knm_number(), kz_term:ne_binary()) -> knm_number:knm_number().
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    update_attributes(Number);
save(Number, _State) ->
    delete(Number).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the number attributes.
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    case feature(Number) of
        'undefined' -> Number;
        _Else -> knm_providers:deactivate_feature(Number, ?KEY)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?KEY).

%%------------------------------------------------------------------------------
%% @doc Update attributes feature on the number
%% @end
%%------------------------------------------------------------------------------
-spec update_attributes(knm_number:knm_number()) -> knm_number:knm_number().
update_attributes(Number) ->
    CurrentAttributes = feature(Number),
    PhoneNumber = knm_number:phone_number(Number),
    AttributesFromDoc = kz_json:get_ne_json_value(?KEY, knm_phone_number:doc(PhoneNumber)),
    NotChanged = kz_json:are_equal(AttributesFromDoc, CurrentAttributes),
    case kz_term:is_empty(AttributesFromDoc) of
        'true' ->
            knm_providers:deactivate_feature(Number, ?KEY);
        'false' when NotChanged ->
            Number;
        'false' ->
            case kz_json:is_true(?FEATURE_ENABLED, AttributesFromDoc) of
                'false' -> knm_providers:deactivate_feature(Number, ?KEY);
                'true' -> knm_providers:activate_feature(Number, {?KEY, AttributesFromDoc})
            end
    end.
