-module(player_2).

-export([start/0,
         start/1,
         add/2,
         list/1]).

-export([loop/1]).

%% @doc start function to initialize the process loop
-spec start() -> pid().
start() ->
  start([]).

%% @doc start function with preloading capability
-spec start([term()]) -> pid().
start(Songs) when is_list(Songs) ->
  spawn(?MODULE, loop, [Songs]).

%% @doc send add message to player process
-spec add(pid(), term()) -> {add_song, pid(), term()}.
add(PlayerPid, Song) ->
  PlayerPid ! {add, self(), Song}.

%% @doc send list message to player process
-spec list(pid()) -> {list, pid()}.
list(PlayerPid) ->
  PlayerPid ! {list, self()}.

%% @doc process loop calls itself to maintain process state
-spec loop([term()]) -> no_return().
loop(Songs) ->
    receive
        {add, From, Song} ->
            %% reply to calling process with a reply tuple
            From ! {song_added, Song},
            loop([Song | Songs]);
        {list, From} ->
            %% reply to calling process with list of songs
            From ! {songs, Songs},
            loop(Songs)
    end.
