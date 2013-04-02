-module(dikdik_db).

-export([simple_query/1
        ,simple_query/2
        ,extended_query/2
        ,extended_query/3]).

-include("dikdik.hrl").

simple_query(Query) ->
    simple_query(Query, false).
simple_query(Query, UseTransaction) ->
    do(fun(C) -> dikdik_db_worker:simple_query(C, Query) end, UseTransaction).

extended_query(Statement, Params) ->
    extended_query(Statement, Params, false).
extended_query(Statement, Params, UseTransaction) ->
    do(fun(C) -> dikdik_db_worker:extended_query(C, Statement, Params) end, UseTransaction).

%%% Internal functions

do(Fun, false) ->
    C = poolboy:checkout(?POOL, true, 5000),
    try
        Fun(C)
    after
        ok = poolboy:checkin(?POOL, C)
    end;
do(Fun, true) ->
    C = poolboy:checkout(?POOL, true, 5000),
    case dikdik_db_worker:simple_query(C, "BEGIN") of
        {'begin',[]} ->
            try 
                Fun(C)
            after
                {rollback,[]} = dikdik_db_worker:simple_query(C, "ROLLBACK"),
                ok = poolboy:checkin(?POOL, C)
            end;
        _ ->
            ok = poolboy:checkin(?POOL, C)
    end.    

