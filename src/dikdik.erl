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

%% API
-export([all/0
        ,all_key/1
        ,create_table/1
        ,find/2
        ,create/2
        ,create/3
        ,update/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% Return all documents in database
-spec all() -> [jsx:json_term()].
all() ->
    [].

%% Return all documents containing Key/Value pair
-spec all_key({Key::binary(), Value::binary()}) -> [jsx:json_text()].
all_key(_Key) ->
    [].

%% Find document with given Id, assumes Id is unique
-spec find(Table::binary(), Id :: binary()) -> jsx:json_text().
find(Table, Id) when is_binary(Id) ->
    {{select, _Rows}, [{{array, Results}}]} =
        dikdik_db:extended_query(<<"SELECT %% hstore(data) FROM ", Table/binary, " WHERE id=$1">>, [Id]),
    jsx:encode(array_to_erl_json(Results)).

%% Create new table named Table
-spec create_table(Table::binary()) -> ok | {error, Error::binary()}.
create_table(Table) when is_binary(Table) ->
    dikdik_db:simple_query(<<"CREATE TABLE ", Table/binary, " (id varchar(256) PRIMARY KEY, data hstore)">>).

%% Create new document with Name, assumes Name does not currently exist
-spec create(Table::binary(), Doc::jsx:json_text()) -> ok | {error, Error::binary()}.
create(Table, Doc) ->
    Values = to_insert_vals(Doc),
    dikdik_db:simple_query(<<"INSERT INTO ", Table/binary," (id, data) VALUES (uuid_generate_v4(), '", Values/binary,"')">>).

-spec create(Table::binary(), Key::binary(), Doc::jsx:json_text()) -> ok | {error, Error::binary()}.
create(Table, Key, Doc) ->
    Values = to_insert_vals(Doc),
    dikdik_db:simple_query(<<"INSERT INTO ", Table/binary," (id, data) VALUES ('", Key/binary, "','", Values/binary,"')">>).

%% Update an already existing document at Id with new document
-spec update(Id::binary(), Doc::jsx:json_text()) -> ok | {error, Error::binary()}.
update(_Id, _Doc) ->
    ok.

%%% Internal functions

to_insert_vals(Doc) ->
    [{K1, V1} | T] = jsx:decode(Doc),
    << <<K1/binary, " => ", (encode_and_escape(V1))/binary>>/binary,
       << <<", ", K/binary, " => \"", (encode_and_escape(V))/binary, "\"" >> || {K, V}  <- T >>/binary >>.

encode_and_escape(B) ->
    B2 = binary:replace(jsx:encode(B), <<"'">>, <<"''">>, [global]),
    binary:replace(B2, <<"\"">>, <<"\\\"">>, [global]).

array_to_erl_json(Array) ->
    array_to_erl_json(Array, []).

array_to_erl_json([], Acc) ->
    Acc;
array_to_erl_json([K, V | T], Acc) ->
    array_to_erl_json(T, [{K, jsx:decode(V)} | Acc]).
