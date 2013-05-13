-module(basic_SUITE).

-export([suite/0
        ,all/0
        ,groups/0
        ,init_per_group/2
        ,end_per_group/2
        ,init_per_testcase/2
        ,end_per_testcase/2]).
-export([create/1
        ,insert_replace_update/1
        ,all_match/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [].

all() ->
    [{group, database_access}].

groups() ->
    [{database_access, [], [create
                           ,insert_replace_update
                           ,all_match]}].

init_per_group(database_access, Config) ->
    os:putenv("DB_POOL_SIZE", "1"),
    os:putenv("DB_MAX_OVERFLOW", "1"),
    dikdik_app:start(),

    TableName = create_random_name("dikdik_ct_test_table_"),
    [{table_name, TableName} | Config].

end_per_group(database_access, Config) ->
    TableName = ?config(table_name, Config),
    dikdik_db:simple_query(<<"DROP TABLE ", TableName/binary>>),
    ok.

init_per_testcase(all_match, Config) ->
    TableName = create_random_name("dikdik_ct_test_table_"),
    ok = dikdik:new(TableName),
    [{all_test_table_name, TableName} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(all_match, Config) ->
    TableName = ?config(all_test_table_name, Config),
    dikdik_db:simple_query(<<"DROP TABLE ", TableName/binary>>),
    ok;
end_per_testcase(_, _Config) ->
    ok.

create(Config) ->
    TableName = ?config(table_name, Config),
    ok = dikdik:new(TableName).

insert_replace_update(Config) ->
    TableName = ?config(table_name, Config),

    Doc1 = <<"{\"test_key_1\":\"test_value\",\"update_me_key\":\"update_me_value\"}">>,
    Doc2 = <<"{\"test_key_2\":2}">>,
    Doc3 = <<"{\"test_key_3\":3}">>,
    Doc4 = <<"{\"test_key_1\":\"test_value\",\"update_me_key\":\"updated\"}">>,

    Id1 = create_random_name("id_"),
    Id2 = create_random_name("id_"),

    ok = dikdik:insert(TableName, Id1, Doc1),
    ok = dikdik:insert(TableName, Id2, Doc2),
    ok = dikdik:replace(TableName, Id2, Doc3),

    ?assert(compare_json(Doc1, dikdik:lookup(TableName, Id1))),
    ?assert(compare_json(Doc3, dikdik:lookup(TableName, Id2))),

    ok = dikdik:update(TableName, Id1, <<"{\"update_me_key\":\"updated\"}">>),

    ?assert(compare_json(Doc4, dikdik:lookup(TableName, Id1))).

all_match(Config) ->
    TableName = ?config(all_test_table_name, Config),

    Doc1 = <<"{\"test_key_1\":\"test_value\",\"test_key_2\":\"test_value_2\"}">>,
    Doc2 = <<"{\"test_key_2\":2}">>,

    ok = dikdik:insert(TableName, Doc1),
    ok = dikdik:insert(TableName, Doc2),

    ?assert(compare_json(<<"[", Doc1/binary, ",", Doc2/binary, "]">>, dikdik:all(TableName))),
    ?assert(compare_json(<<"[", Doc1/binary, "]">>, dikdik:match(TableName, [{<<"test_key_2">>, <<"test_value_2">>}]))).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

compare_json(J1, J2) ->
    sort(jsx:decode(J1)) == sort(jsx:decode(J2)).

sort(L = [X | _]) when is_list(X) ->
    [sort(Y) || Y <- L];
sort(L = [X | _]) when is_tuple(X)->
    lists:keysort(1, L).

create_random_name(Name) ->
    random:seed(erlang:now()),
    list_to_binary(Name ++ erlang:integer_to_list(random:uniform(1000000))).
