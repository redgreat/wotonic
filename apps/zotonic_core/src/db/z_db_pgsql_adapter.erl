-module(z_db_pgsql_adapter).

-behaviour(z_db_adapter).

-export([
    ensure_all_started/0,
    test_connection/1,
    squery/3,
    equery/4,
    execute_batch/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc 启动 PostgreSQL 驱动所需应用
ensure_all_started() ->
    z_db_pgsql:ensure_all_started().

%% @doc 测试连接
test_connection(Args) ->
    z_db_pgsql:test_connection(Args).

%% @doc 执行无参数查询
squery(Worker, Sql, Timeout) ->
    z_db_pgsql:squery(Worker, Sql, Timeout).

%% @doc 执行带参数查询
equery(Worker, Sql, Params, Timeout) ->
    z_db_pgsql:equery(Worker, Sql, Params, Timeout).

%% @doc 批量执行
execute_batch(Worker, Sql, Params, Timeout) ->
    z_db_pgsql:execute_batch(Worker, Sql, Params, Timeout).

