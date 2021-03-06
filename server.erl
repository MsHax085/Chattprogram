-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% ---------------------------------------------------------------------------
%% Produce initial state
% Creates a record with dicts clients and channels.
initial_state(ServerName) ->
  io:format("Server `~p' created.", [ServerName]),
  #server_st{clients = dict:new(), channels = dict:new()}.

%% ---------------------------------------------------------------------------
%% Connect to server
% Tries to connect to the given server (only shire exists)
% and the message "User ~p already connected" will be sent if the user is already connected,
% and "User ~p connected to the server and was inserted to userlist~n" when the user is connected
% with the server.
handle(St, {connect, Pid, Nick}) ->
	case dict:is_key(Nick, St#server_st.clients) of
		true ->
			io:fwrite("Nick ~p is taken", [Nick]),
			{reply, nick_taken, St};
		false ->
			io:fwrite("User ~p connected to the server and was inserted to userlist~n", [Nick]),
			{reply, ok, St#server_st{clients = dict:store(Nick, Pid, St#server_st.clients)}}
	end;

%% ---------------------------------------------------------------------------
%% Disconnect from server
% Tries to disconnect from the current server 
% and the message "User ~p disconnected from the server and was removed from userlist~n" will be sent
% when the user has been disconnected from the current server. "not_connected" is sent when the user
% is not connected to any server.
handle(St, {disconnect, Nick}) ->
	case dict:is_key(Nick, St#server_st.clients) of
		true ->
			io:fwrite("User ~p disconnected from the server and was removed from userlist~n", [Nick]),
			{reply, ok, St#server_st{clients = dict:erase(Nick, St#server_st.clients)}};
		false ->
			{reply, not_connected, St}
	end;
	
%% ---------------------------------------------------------------------------
%% Join channel
% Creates a new channel if it does not exist. If the channel already exists
% "join" is sent to the client.
handle(St, {join_channel, Channel}) ->
	case dict:is_key(Channel, St#server_st.channels) of
		true ->
			% Channel exist
			{reply, join, St};
		false -> 
			% Start new channel
			genserver:start(list_to_atom(Channel), channel:initial_state(Channel), fun channel:handle/2),
			{reply, join, St#server_st{channels = dict:store(Channel, 0, St#server_st.channels)}}
	end.

%% ---------------------------------------------------------------------------
