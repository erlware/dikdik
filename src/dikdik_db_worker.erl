-module(dikdik_db_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1
        ,simple_query/2
        ,extended_query/3]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

simple_query(Pid, Query) ->
    gen_server:call(Pid, {simple_query, Query}).

extended_query(Pid, Stmt, Params) ->
    gen_server:call(Pid, {extended_query, Stmt, Params}).

init(Options) ->
    Conn = {pgsql_connection, Pid} = pgsql_connection:open(Options),
    link(Pid),
    {ok, #state{conn=Conn}}.

handle_call({simple_query, Query}, _From, #state{conn=Conn}=State) ->
    case pgsql_connection:simple_query(Query, Conn) of
        {error, Reason} ->
            lager:info("at=simple_query error=~p", [Reason]),
            {stop, Reason, State};
        Result ->
            {reply, Result, State}
    end;
handle_call({extended_query, Stmt, Params}, _From, #state{conn=Conn}=State) ->
    case pgsql_connection:extended_query(Stmt, Params, Conn) of
        {error, Reason} ->
            lager:info("at=extended_query error=~p", [Reason]),
            {stop, Reason, State};
        Result ->
            {reply, Result, State}
    end;
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql_connection:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
