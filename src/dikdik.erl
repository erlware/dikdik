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
        ,match/2
        ,new/1
        ,lookup/2
        ,insert/2, insert/3
        ,update/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% Return all documents in database
-spec all() -> [jsx:json_term()].
all() ->
    [].

%% Return all documents containing Key/Value pairs
-spec match(Table::binary(), Pairs::[{Key::binary(), Value::binary()}]) -> [jsx:json_text()].
match(Table, Pairs)
  when is_binary(Table),
       is_list(Pairs)->
    {WhereStr, WhereList} = build_where(Pairs),
    {{select, _Rows}, Results} =
        dikdik_db:extended_query(<<"SELECT %% hstore(data) FROM ", Table/binary,
                                   WhereStr/binary>>, WhereList),
    [jsx:encode(array_to_erl_json(X)) || {{array, X}} <- Results].

%% Find document with given Id, assumes Id is unique
-spec lookup(Table::binary(), Id :: binary()) -> jsx:json_text().
lookup(Table, Id)
  when is_binary(Table),
       is_binary(Id) ->
    {{select, _Rows}, [{{array, Results}}]} =
        dikdik_db:extended_query(<<"SELECT %% hstore(data) FROM ", Table/binary,
                                   " WHERE id=$1">>, [Id]),
    jsx:encode(array_to_erl_json(Results)).

%% Create new table named Table
-spec new(Table::binary()) -> ok | {error, Error::binary()}.
new(Table) when is_binary(Table) ->
    dikdik_db:simple_query(<<"CREATE TABLE ", Table/binary, " (id varchar(256) PRIMARY KEY, data hstore)">>).

%% Create new document with Name, assumes Name does not currently exist
-spec insert(Table::binary(), Doc::jsx:json_text()) -> ok | {error, Error::binary()}.
insert(Table, Doc)
  when is_binary(Table),
       is_binary(Doc) ->
    Values = to_insert_vals(Doc),
    dikdik_db:simple_query(<<"INSERT INTO ", Table/binary," (id, data) VALUES (uuid_generate_v4(), '", Values/binary,"')">>).

-spec insert(Table::binary(), Key::binary(), Doc::jsx:json_text()) -> ok | {error, Error::binary()}.
insert(Table, Key, Doc)
  when is_binary(Table),
       is_binary(Key),
       is_binary(Doc) ->
    Values = to_insert_vals(Doc),
    dikdik_db:simple_query(<<"INSERT INTO ", Table/binary," (id, data) VALUES ('", Key/binary, "','", Values/binary,"')">>).

%% Update an already existing document at Id with new document
-spec update(Id::binary(), Doc::jsx:json_text()) -> ok | {error, Error::binary()}.
update(_Id, _Doc) ->
    ok.

%%% Internal functions

build_where(Pairs) ->
    build_where(Pairs, {<<"">>, []}).
build_where([], {WhereStr, WhereList}) ->
    {WhereStr, lists:reverse(WhereList)};
build_where([{K, V} | T], {WhereStr, WhereList}) ->
    KNum = list_to_binary(integer_to_list(length(WhereList)+1)),
    VNum = list_to_binary(integer_to_list(length(WhereList)+2)),
    Str = case KNum of
              <<"1">> ->
                  <<" WHERE data->$", KNum/binary, "=$", VNum/binary>>;
              _KNum ->
                  <<" AND data->$", KNum/binary, "=$", VNum/binary>>
          end,
    NewWhereStr = <<WhereStr/binary, Str/binary>>,
    NewWhereList = [jsx:encode(V), K | WhereList],
    build_where(T, {NewWhereStr, NewWhereList}).

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
