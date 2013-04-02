%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%---------------------------------------------------------------------------
%%% @author Erlware <core@erlware.org>
%%% @copyright (C) 2013 Erlware, LLC.
%%%
%%% @doc
%%%  Erlang interface for Heroku's PostgreSQL
%%% @end
-module(dikdik_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-export([config/0
         ,config/1
         ,config/2
         ,set_config/2
        ]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start() ->
    application:set_env(lager, handlers, {handlers, [
                                                    {lager_console_backend, [info]}
                                                    ]}),

    cache_os_envvars(),
    start_deps(dikdik, permanent).

%% @private
start_deps(App, Type) ->
    case application:start(App, Type) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            start_deps(Dep, Type),
            start_deps(App, Type)
    end.

%% @private
start(_StartType, _StartArgs) ->
    case dikdik_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            lager:error("Failed to start dikdik_sup: ~p", [Error]),
            Error
    end.

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cache_os_envvars() ->
    cache_os_envvars([
                     {db_url, "DATABASE_URL"}
                     ,{db_pool_size, "DB_POOL_SIZE"}
                     ,{db_max_overflow, "DB_MAX_OVERFLOW"}
                     ]).

cache_os_envvars([]) ->
    ok;
cache_os_envvars([{Key, OsKey}|Tail]) when is_atom(Key) ->
    Val =
        case os:getenv(OsKey) of
            false -> "";
            OsVal -> OsVal
        end,
    set_config(Key, Val),
    cache_os_envvars(Tail).

config(Key, Default) when is_atom(Key) ->
    case application:get_env(dikdik, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) when is_atom(Key) ->
    case application:get_env(dikdik, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.

config() ->
    application:get_all_env(dikdik).

set_config(KeyS, Value) when is_list(KeyS) ->
    set_config(list_to_atom(KeyS), Value);
set_config(Key, Value) when is_atom(Key) ->
    application:set_env(dikdik, Key, Value).
