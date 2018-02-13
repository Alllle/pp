-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    server,
    channels
}).

initial_state(ServerAtom) ->
    #server_st{
        server = ServerAtom,
        channels = []
    }.
% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    PID = genserver:start(ServerAtom, initial_state(ServerAtom), fun body/2)
    PID.

body(State, {join, Channel, Client}) ->
  
%handles joining channels

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
