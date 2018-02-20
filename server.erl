-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    server, % server name.
    channels % all the channels
}).
-record(channel_st, {
    channel, % atom of the server
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
  Condition = not list:member(Channel, State#server_st.channels),
  if Condition ->
    genserver:start(Channel, initial_channel_state(Channel)), fun channel_handler/2,
    NewState = State#server_st{channels = State#server_st.channels ++ [Channel]}
  end,
  case genserver:request(Channel, {join, Client}) of
    ok -> {reply, ok, NewState};
    user_already_joined -> {reply, user_already_joined, NewState}
  end;
handler(Whatever, _) ->
  not_implemented.

channel_handler(State, {join, Client}) ->
  Reply = lists:member(Client, State#channel_st.users),
  if
    Reply =:= true -> {reply, user_already_joined, State};
    true -> NewState = State#channel_st{users = State#channel_st.users ++ [Client]},
      {reply, ok, NewState}
  end.




% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
