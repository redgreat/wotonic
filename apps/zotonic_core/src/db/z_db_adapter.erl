-module(z_db_adapter).

-callback ensure_all_started() -> ok | {error, term()}.
-callback test_connection(Args :: list()) -> ok | {error, term()}.
-callback squery(Worker :: pid(), Sql :: string() | binary(), Timeout :: pos_integer()) -> term().
-callback equery(Worker :: pid(), Sql :: string() | binary(), Params :: list(), Timeout :: pos_integer()) -> term().
-callback execute_batch(Worker :: pid(), Sql :: string() | binary(), Params :: list(), Timeout :: pos_integer()) -> term().

