-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
  io:format("Server `~p' created.", [ServerName]),
  #server_st{clients = dict:new(), channels = dict:new()}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.
%% Connect client

handle(St, {connect, Pid, Nick}) ->
	case dict:is_key(Pid, St#server_st.clients) of
		true ->
			io:fwrite("User ~p already connected", [Nick]),
			{reply, user_already_connected, St};
		false ->
			io:fwrite("User ~p connected to the server and was inserted to userlist~n", [Nick]),
			{reply, ok, St#server_st{clients = dict:store(Pid, Nick, St#server_st.clients)}}
	end;
		
handle(St, {disconnect, Pid, Nick}) ->
	%% fixar , leave_channels_first, server_not_reached senare
	io:fwrite("User ~p disconnected from the server and was removed from userlist~n", [Nick]),
	{reply, ok, St#server_st{clients = dict:erase(Pid, St#server_st.clients)}};
	
handle(St, {join_channel, Channel}) ->
	case dict:is_key(Channel, St#server_st.channels) of
		true ->
			% Chennel exist
			{reply, join, St};
		false ->
			% Start new channel
			genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2),
			{reply, join, St#server_st{channels = dict:store(Channel, St#server_st.channels)}}
	end.