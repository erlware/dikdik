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
-export([start_link/4,
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

-record(state, {db}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(Server::binary(), Port::integer(), Opts::list(), DB::binary()) -> {ok, Pid::pid()} | ignore | {error, Error :: term() | {already_started, Pid::pid()}}.
start_link(Server, Port, Opts, DB) ->
    gen_server:start_link(?MODULE, [Server, Port, Opts, DB], []).

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

%% **TODO Change to connect to PostgreSQL
%% @private
init([Server, Port, Opts, DB]) ->
    CouchServer = couchbeam:server_connection(Server, Port, "", Opts),
    {ok, CouchDB} = couchbeam:open_db(CouchServer, DB),
    {ok, #state{db=CouchDB}}.

%% @private
handle_call(all, _From, #state{db=DB}=State) ->
    Docs = get_docs(DB, [include_docs]),
    {reply, Docs, State};
handle_call({all_key, Key}, _From, #state{db=DB}=State) ->
    Docs = get_docs(DB, [include_docs]),
    Docs2 = filter_by_key(Docs, Key),
    {reply, Docs2, State};
handle_call({find, ID}, _From, #state{db=DB}=State) ->
    Doc = get_docs(DB, [{key, ID}, include_docs]),
    {reply, Doc, State};
handle_call({create, Doc}, _From, #state{db=DB}=State) ->
    {ok, Doc1} = couchbeam:save_doc(DB, Doc),
    {reply, Doc1, State};
handle_call({update, IDBinary, NewDoc}, _From, #state{db=DB}=State) ->
    {ok, Doc} = couchbeam:open_doc(DB, IDBinary),
    NewDoc2 = couchbeam_doc:set_value(<<"_id">>, IDBinary, {NewDoc}),
    Rev = couchbeam_doc:get_rev(Doc),
    NewDoc3 = couchbeam_doc:set_value(<<"_rev">>, Rev, NewDoc2),
    {ok, Doc1} = couchbeam:save_doc(DB, NewDoc3),
    {reply, Doc1, State};
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

get_docs(DB, Options) ->
    case couchbeam_view:fetch(DB, 'all_docs', Options) of
        {ok, Results} ->
            Results;
        _ ->
            error
    end.

filter_by_key(Docs, Key) ->
    filter_by_key(Docs, Key, []).
filter_by_key([], _Key, R) ->
    lists:reverse(R);
filter_by_key([Doc|T], {Key, Value}, R) ->
    case lists:keyfind(Key, 1, Doc) of
        {Key, Value}  ->
            filter_by_key(T, {Key, Value}, [Doc|R]);
        _ -> filter_by_key(T, {Key, Value}, R)
    end.
