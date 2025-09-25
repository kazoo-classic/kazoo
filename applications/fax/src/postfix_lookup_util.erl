%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2025-2025, RuhNet
%%% @doc Postfix tcp_table server lookup common utility functions
%%%
%%% @author Ruel Tmeizeh (www.ruhnet.co)
%%% @end
%%%-----------------------------------------------------------------------------
-module(postfix_lookup_util).

-export([accept/3]).
-export([check_fax_domain/1]).

-include("fax.hrl").

-define(TIMEOUT, 8 * ?MILLISECONDS_IN_SECOND).

-spec accept(gen_tcp:socket(), fun(), atom()) -> 'ok' | {'error', any()}.
accept(LSocket, ServeFunc, ModuleName) ->
    case gen_tcp:accept(LSocket) of
        {'ok', AcceptSocket} ->
            case handle_request(AcceptSocket, ServeFunc) of
                'ok' -> 'ok';
                {'error', Reason} -> lager:debug("~p failed to lookup and serve data: ~p", [ModuleName, Reason])
            end,
            gen_tcp:close(AcceptSocket);
        {'error', Reason} -> lager:debug("~p accept connection failed: ~p", [ModuleName, Reason])
    end.

-spec handle_request(gen_tcp:socket(), fun()) -> 'ok' | {'error', any()}.
handle_request(Socket, ServeFunc) ->
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {'ok', ReqData} ->
            case trim_newline(ReqData) of
                <<"get UP">> -> %% health check
                    gen_tcp:send(Socket, <<"500 UP\n">>);
                <<"get ", ReqKey/binary>> -> %% valid request
                    _ = gen_tcp:shutdown(Socket, 'read'), %% close the read part of the socket, since that is done
                    ServeFunc(Socket, ReqKey);
                _ ->
                    _ = gen_tcp:send(Socket, <<"400 bad_request\n">>),
                    {'error', 'invalid_request_format'}
            end;
        {'error', Reason}=Error ->
            lager:debug("postfix lookup failed to receive data from client: ~p", [Reason]),
            Error
    end.

-spec check_fax_domain(kz_term:ne_binary()) -> 'ok' | 'not_found' | kz_datamgr:get_results_return().
check_fax_domain(Domain) ->
    DomainL = kz_term:to_lower_binary(Domain),
    case kz_cache:peek_local(?CACHE_NAME, {'fax_domain', DomainL}) of
        {'ok', 'ok'} -> 'ok';
        {'error', _} ->
            case lookup_fax_domain(DomainL) of
                'ok' ->
                    kz_cache:store_local(?CACHE_NAME, {'fax_domain', Domain}, 'ok'),
                    'ok';
                NotFoundOrErr ->
                    NotFoundOrErr
            end
    end.

-spec lookup_fax_domain(kz_term:ne_binary()) -> 'ok' | 'not_found' | kz_datamgr:get_results_return().
lookup_fax_domain(Domain) ->
    ViewOptions = [{'key', Domain}],
    case kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', [_|_]} -> 'ok';
        _ -> lookup_account_realm(Domain)
    end.

-spec lookup_account_realm(kz_term:ne_binary()) -> 'ok' | 'not_found' | kz_datamgr:get_results_return().
lookup_account_realm(Domain) ->
    ViewOptions = [{'key', Domain}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_realm">>, ViewOptions) of
        {'ok', []} -> 'not_found';
        {'ok', [_|_]} -> 'ok';
        {'error', _E}=Error -> Error
    end.

-spec trim_newline(kz_term:ne_binary()) -> kz_term:ne_binary().
trim_newline(Binary) ->
    Size = byte_size(Binary) - 1,
    case Binary of
        <<Req:(Size)/binary, $\n>> -> Req;
        _ -> Binary
    end.
