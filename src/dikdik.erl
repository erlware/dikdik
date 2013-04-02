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
-module(dikdik).

-behaviour(gen_server).

%% API
-export([start_link/0,
         all/1,
         all_key/2,
         find/2,
         create/2,
         update/3,
         terminate/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, Pid::pid()} | ignore | {error, Error :: term() | {already_started, Pid::pid()}}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Return all documents in database
-spec all(PID::pid()) -> [jsx:json_term()].
all(PID) ->
    gen_server:call(PID, all).

%% Return all documents containing Key/Value pair
-spec all_key(PID::pid(), {Key::binary(), Value::binary()}) -> [jsx:json_term()].
all_key(PID, Key) ->
    gen_server:call(PID, {all_key, Key}).

%% Find document with given ID, assumes ID is unique
-spec find(PID::pid(), ID::binary()) -> jsx:json_term().
find(PID, ID) ->
    gen_server:call(PID, {find, ID}).

%% Create new document with ID, assumes ID does not currently exist
-spec create(PID::pid(), Doc::jsx:json_term()) -> ok | {error, Error::binary()}.
create(PID, Doc) ->
    gen_server:call(PID, {create, Doc}).

%% Update an already existing document at ID with new document
-spec update(PID::pid(), ID::binary(), Doc::jsx:json_term()) -> ok | {error, Error::binary()}.
update(PID, ID, Doc) ->
    gen_server:call(PID, {update, ID, Doc}).

-spec terminate(PID::pid()) -> ok | {error, Error::binary()}.
terminate(PID) ->
    gen_server:call(PID, terminate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call(all, _From, State) ->
    {reply, [], State};
handle_call({all_key, _Key}, _From, State) ->
    {reply, [], State};
handle_call({find, _ID}, _From, State) ->
    {reply, <<>>, State};
handle_call({create, _Doc}, _From, State) ->
    {reply, <<>>, State};
handle_call({update, _IDBinary, _NewDoc}, _From, State) ->
    {reply, <<>>, State};
handle_call(terminate, _From, State) ->
    {stop, normal, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    lager:info("dikdik Info: ~p, State: ~p", [Info, State]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
