#!/usr/bin/env escript
%%! +A0 -sname kazoo_xref
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main([TagsFile]) ->
    io:format("~n[DEBUG] Starting tags generation at ~p~n", [calendar:local_time()]),
    AppDirs = lists:foldl(fun add_app_dirs/2, [], kz_ast_util:project_apps()),
    io:format("[DEBUG] Collected app dirs, starting path resolution at ~p~n", [calendar:local_time()]),
    Paths = [app_path(App) || App <- lists:usort(AppDirs)],
    io:format("[DEBUG] Resolved ~p paths, starting tags:subdirs at ~p~n", [length(Paths), calendar:local_time()]),
    io:format("[DEBUG] Paths: ~p~n", [Paths]),
    Result = tags:subdirs(Paths, [{'outfile', TagsFile}]),
    io:format("[DEBUG] Finished tags:subdirs at ~p with result ~p~n", [calendar:local_time(), Result]),
    Result.

add_app_dirs(App, Dirs) ->
    case application:load(App) of
        'ok' -> add_app_dirs(App, Dirs, application:get_key(App, 'applications'));
        {'error', {'already_loaded', App}} ->
            Dirs;
        {'error', _E} ->
            io:format("failed to load app ~p: ~p~n", [App, _E]),
            Dirs
    end.

add_app_dirs(App, Dirs, {'ok', DepApps}) ->
    _ = [application:load(DepApp)
         || DepApp <- DepApps,
            not lists:member(DepApp, Dirs)
        ],
    Dirs ++ [App | DepApps];
add_app_dirs(_App, Dirs, _Else) ->
    io:format("failed to list dep apps for ~s: ~p~n", [_App, _Else]),
    Dirs.

app_path(App) ->
    case application:get_key(App, 'modules') of
        {'ok', [M | _]} -> 
            filename:dirname(filename:dirname(code:which(M)));
            io:format("Info: Application ~p has modules added to TAGS~n", [App]),
        {'ok', []} ->
            io:format("Warning: Application ~p has empty modules list, skipping~n", [App]),
            ".";
        undefined ->
            io:format("Warning: Application ~p has no modules key, assuming empty modules list~n", [App]),
            ".";
        Other ->
            io:format("Warning: Unexpected result for modules of application ~p: ~p~n", [App, Other]),
            "."
    end.
