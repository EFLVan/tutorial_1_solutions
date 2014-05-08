-module(player_5).

-export([start/0,
         start/1,
         add/2,
         list/1,
         delete/2,
         play/1
        ]).

-export([loop/1,
         play_song/1]).

%% @doc start function to initialize the process loop
-spec start() -> pid().
start() ->
  start([]).

%% @doc start function with preloading capability
-spec start([term()]) -> pid().
start(Songs) when is_list(Songs) ->
    spawn(?MODULE, loop, [{Songs, undefined}]).

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

%% @doc send the play message to player process and handle the response
-spec play(pid()) -> {playing, term()}
                          | end_of_playlist
                          | {currently_playing, pid()}.
play(PlayerPid) ->
  PlayerPid ! {koolplayer, self(), play},
  receive
    {playing, _SongPid, _Song} = Response ->
      Response;
    end_of_playlist = Response ->
      io:format("Reached end of playlist~n"),
      Response;
    {currently_playing, _SongPid} = Response ->
      io:format("Busy playing~n"),
      Response
  after 500 ->
          {error, no_reply}
  end.


loop(State) ->
    receive
        %% Match on our message shape
        {koolplayer, From, Cmd} ->
             %% Call the handle_message function with the Command
             {reply, Message, NewState} = handle_message(Cmd, State),
             From ! Message,
             loop(NewState);
        %% Match on the monitor message (when the playing song process
        %% goes down)
        {'DOWN', Ref, process, Pid, normal} ->
          NewState = handle_monitor_down(Pid, Ref, State),
          loop(NewState)
    end.

handle_message({add, Song}, {Songs, CurrentSong}) ->
    {reply, {song_added, Song}, {[Song | Songs], CurrentSong}};
handle_message(list, {Songs, _} = State) ->
    {reply, {songs, Songs}, State};
handle_message({delete, Song}, {Songs, CurrentSong}) ->
    {Status, NewSongs} = maybe_delete(Song, Songs),
    {reply, {Status, Song}, {NewSongs, CurrentSong}};
%% Only play a song when there is no song playing
handle_message(play, {[NextSong|Songs], undefined}) ->
    % Spawn a song process
    SongPid = spawn(?MODULE, play_song, [NextSong]),
    % Monitor it
    SongRef = monitor(process, SongPid),
    % Update the state with information on the currently playing son
    {reply, {playing, SongPid, NextSong}, {Songs, {SongPid, SongRef}}};
%% Reply with information that there are no songs left to play
handle_message(play, {[],_}=State) ->
    {reply, end_of_playlist, State};
%% Reply with information on what song we're playing
handle_message(play, {_, {Pid, _}}=State) ->
    {reply, {currently_playing, Pid}, State}.

%% Handle the monitor down message, we need to make sure we're getting
%% the down message from the correct process. This is to prevent a race
%% condition.
handle_monitor_down(Pid, Ref, {Songs, {Pid, Ref}}) ->
    {reply, _, NewState} = handle_message(play, {Songs, undefined}),
    NewState;
%% If we got a down message for a different process, we will just
%% ignore it - another song is already playing.
handle_monitor_down(_Pid, _Ref, {_,_}=State) ->
    State.

play_song(Song) ->
    io:format("Playing song: ~p~n", [Song]),
    receive
    after 1000 ->
            ok
    end.

%% Private functions
maybe_delete(Song, Songs) ->
    case lists:member(Song, Songs) of
        true ->
            {song_deleted, lists:delete(Song, Songs)};
        _ ->
            {no_such_song, Songs}
    end.
