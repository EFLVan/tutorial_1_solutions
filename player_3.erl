-module(player_3).

-export([start/0,
         start/1,
         add/2,
         list/1,
         delete/2]).

-export([loop/1]).

%% @doc start function to initialize the process loop
-spec start() -> pid().
start() ->
  start([]).

%% @doc start function with preloading capability
-spec start([term()]) -> pid().
start(Songs) when is_list(Songs) ->
  spawn(?MODULE, loop, [Songs]).

%% @doc send add message to player process and handle the response
-spec add(pid(), term()) -> {song_added, term()}.
add(PlayerPid, Song) ->
  PlayerPid ! {add, self(), Song},
  receive
    {song_added, Song} = Response ->
      io:format("Song added: ~p~n", [Song]),
      Response
  end.

%% @doc send list message to player process and handle the response
-spec list(pid()) -> {songs, [term()]}.
list(PlayerPid) ->
  PlayerPid ! {list, self()},
  receive
    {songs, Songs} = Response ->
      io:format("Songs: ~p~n", [Songs]),
      Response
  end.

%% @doc send delete message to player process and handle the response
-spec delete(pid(), term()) -> {song_deleted, term()}
                                    | {no_such_song, term()}.
delete(PlayerPid, Song) ->
  PlayerPid ! {delete, self(), Song},
  receive
    {song_deleted, Song} = Response ->
      io:format("Song deleted: ~p~n", [Song]),
      Response;
    {no_such_song, Song} = Response ->
      io:format("No such song: ~p~n", [Song]),
      Response
  end.


loop(Songs) ->
    receive
        {add, From, Song} ->
            From ! {song_added, Song},
            loop([Song | Songs]);
        {list, From} ->
            From ! {songs, Songs},
            loop(Songs);
        {delete, From, Song} ->
            {Status, Songs1} = maybe_delete(Song, Songs),
            From ! {Status, Song},
            loop(Songs1)
    end.

%% Private functions
maybe_delete(Song, Songs) ->
    case lists:member(Song, Songs) of
        true ->
            {song_deleted, lists:delete(Song, Songs)};
        _ ->
            {no_such_song, Songs}
    end.
