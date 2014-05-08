-module(player_4).

-export([start/0,
         start/1,
         add/2,
         list/1,
         delete/2
        ]).

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
    PlayerPid ! {koolplayer, self(), {add, Song}},
    receive
      {song_added, Song} = Response ->
          io:format("Song added: ~p~n", [Song]),
          Response
  after 500 ->
          {error, no_reply}
  end.

%% @doc send list message to player process and handle the response
-spec list(pid()) -> {songs, [term()]}.
list(PlayerPid) ->
    PlayerPid ! {koolplayer, self(), list},
    receive
      {songs, Songs} = Response ->
          io:format("Songs: ~p~n", [Songs]),
          Response
  after 500 ->
          {error, no_reply}
  end.

%% @doc send delete message to player process and handle the response
-spec delete(pid(), term()) -> {song_deleted, term()}
                                   | {no_such_song, term()}.
delete(PlayerPid, Song) ->
    PlayerPid ! {koolplayer, self(), {delete, Song}},
    receive
      {song_deleted, Song} = Response ->
          io:format("Song deleted: ~p~n", [Song]),
          Response;
      {no_such_song, Song} = Response ->
          io:format("No such song: ~p~n", [Song]),
          Response
  after 500 ->
          {error, no_reply}
  end.


loop(Songs) ->
    receive
        %% Match on our message shape
        {koolplayer, From, Cmd} ->
            %% Call the handle_message function with the Command
            {reply, Message, NewSongs} = handle_message(Cmd, Songs),
            From ! Message,
            loop(NewSongs)
    end.

handle_message({add, Song}, Songs) ->
    {reply, {song_added, Song}, [Song | Songs]};
handle_message(list, Songs) ->
    {reply, {songs, Songs}, Songs};
handle_message({delete, Song}, Songs) ->
    {Status, NewSongs} = maybe_delete(Song, Songs),
    {reply, {Status, Song}, NewSongs}.

%% Private functions
maybe_delete(Song, Songs) ->
    case lists:member(Song, Songs) of
        true ->
            {song_deleted, lists:delete(Song, Songs)};
        _ ->
            {no_such_song, Songs}
    end.
