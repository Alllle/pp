-module(server).
-export([start/1, stop/1]).

-record(server_st, {
  server, % server name.
  channels % all the channels
}).
-record(channel_st, {
  channel, % atom of the channel
  users % nick/username of all the users of this channel
}).

initial_server_state(ServerAtom) ->
  #server_st{
    server = ServerAtom,
    channels = []
  }.

initial_channel_state(ChannelAtom) ->
  #channel_st{
    channel = ChannelAtom,
    users = []
  }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
  PID = genserver:start(ServerAtom, initial_server_state(ServerAtom), fun handler/2),
  PID.

handler(State, {join, Channel, Client}) ->
  ChannelAtom = list_to_atom(Channel),
  Condition = not lists:member(ChannelAtom, State#server_st.channels), %checks if the channel exists
  if Condition ->
    genserver:start(ChannelAtom, initial_channel_state(ChannelAtom), fun channel_handler/2),
    NewState = State#server_st{channels = [ChannelAtom | State#server_st.channels]};
    true -> NewState = State
  end,
  case genserver:request(ChannelAtom, {join, Client}) of
    ok -> {reply, ok, NewState};
    user_already_joined -> {reply, user_already_joined, NewState}
  end.

channel_handler(State, {join, Client}) ->
  Condition = lists:member(Client, State#channel_st.users), %Checks if the client is in the channel
  if Condition -> {reply, user_already_joined, State};
    true -> NewState = State#channel_st{users = [Client | State#channel_st.users]},
      {reply, ok, NewState}
  end;

channel_handler(State, {leave, Client}) ->
  Condition = not lists:member(Client, State#channel_st.users),
  if Condition -> {reply, user_not_joined, State};
    true -> NewState = State#channel_st{users = lists:delete(Client, State#channel_st.users)},
      {reply, ok, NewState}
  end;


channel_handler(State, {message_send, Msg, Client, Nick}) ->
  Condition = lists:member(Client, State#channel_st.users), %Checks if client is in the channel
  if Condition ->
    %For each client in channel_st.users, spawn a process that sends the message using genserver
    %Here is where we use concurrency
    lists:foreach(fun(Pid) ->
      spawn(fun() -> genserver:request(Pid, {message_receive, atom_to_list(State#channel_st.channel), Nick, Msg}) end) end,
      lists:delete(Client, State#channel_st.users)),
    {reply, ok, State};
    true -> {reply, user_not_joined, State}
  end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
  genserver:stop(ServerAtom),
  ok.
