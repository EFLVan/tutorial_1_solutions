-module(player_1).

-export([start/0]).

-export([loop/1]).

%% @doc start function to initialize the process loop
-spec start() -> pid().
start() ->
  spawn(?MODULE, loop, [[]]).

%% @doc process loop calls itself to maintain process state
-spec loop([term()]) -> no_return().
loop(Songs) ->
    receive
        Song ->
            io:format("Add song: ~p~n", [Song]),
            loop([Song | Songs])
    end.
