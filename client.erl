-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% ---------------------------------------------------------------------------
%% Produce initial state
% Creates a record with different states like gui, nick, connected, serverRef, channels.
initial_state(Nick, GUIName) ->
    #client_st {gui = GUIName, nick = Nick, connected = false, serverRef = false, channels = dict:new()}.

%% ---------------------------------------------------------------------------
%% Connect to server
% Tries to connect to the given server (only shire exists)
% and the error "User already connected" will be sent if the user is already connected,
% and the error "Server not reached" will be sent when the server is not reached.
handle(St, {connect, Server}) ->
	case St#client_st.connected of
		true ->
			{reply, {error, user_already_connected, "User already connected"}, St};
		false ->
			ServerAtom = list_to_atom(Server),
			try genserver:request(ServerAtom, {connect, self(), St#client_st.nick}) of
				ok ->
					{reply, ok, St#client_st{connected = true, serverRef = Server}};
				nick_taken ->
					{reply, {error, nick_taken, "Nick is taken"}, St}
				catch
					_:_ -> 
						{reply, {error, server_not_reached, "Server not reached"}, St}
			end
	end;

%% ---------------------------------------------------------------------------
%% Disconnect from server
% Send disconnect request to server specified by the serverRef atom in
% record #client_st of client St (client name).
% If ok => Update record #client_st
% The error "Leave all channels first" will be sent if the user is trying to disconnect
% when he haven't left all channels. The error "Could not disconnect from server" will be sent when the server is not reached.
% And at last, the error "User not connected" will be sent if the user isn't connected to any server.
handle(St, disconnect) ->
	case St#client_st.connected of
		true ->
			case dict:is_empty(St#client_st.channels) of
				false ->
					{reply, {error, leave_channels_first , "Leave all channels first"}, St};
				true ->
					ServerAtom = list_to_atom(St#client_st.serverRef),
					try genserver:request(ServerAtom, {disconnect, St#client_st.nick}) of
						ok ->
							{reply, ok, St#client_st{connected = false, serverRef = false}}
						catch
							_:_ ->
								{reply, {error, user_not_connected , "Could not disconnect from server"}, St}
					end
				end;
		false ->
			{reply, {error, user_not_connected, "User not connected"}, St}
	end;

%% ---------------------------------------------------------------------------
%% Join channel
% Trying to join a given channel. Will give the error "Not connected to any server!"
% if the user isn't connected to any server. Also gives the error "User is already in channel!"
% if the user is trying to join a channel which he already is in. And the error
% "Could not reach server!" will be sent if the server isn't reachable.
handle(St, {join, Channel}) ->
	case St#client_st.connected of
		false ->
			{reply, {error, not_connected, "Not connected to any server!"}, St};
		_ ->
			ServerAtom = list_to_atom(St#client_st.serverRef),
			try genserver:request(ServerAtom, {join_channel, Channel}) of
				join ->
					ChannelAtom = list_to_atom(Channel),
					Response2 = genserver:request(ChannelAtom, {join_channel, self(), St#client_st.nick}),
					case Response2 of
						already_in_channel ->
							{reply, {error, user_already_joined, "User is already in channel!"}, St};
						ok ->
							% Add channel to client list
							{reply, ok, St#client_st{channels = dict:store(Channel, 0, St#client_st.channels)}}
					end
				catch
					_:_ ->
						{reply, {error, no_connection, "Could not reach server!"}, St}
			end		
	end;

%% ---------------------------------------------------------------------------
%% Leave channel
% Tries to leave the current channel and will give the error "User has not joined this channel"
% if the user isn't in the current channel.
handle(St, {leave, Channel}) ->
	ChannelAtom = list_to_atom(Channel),
	Response = genserver:request(ChannelAtom, {leave_channel, self()}),
	case Response of
		ok ->
			{reply, ok, St#client_st{channels = dict:erase(Channel, St#client_st.channels)}};
		no_user_of_channel ->
			{reply, {error, user_not_joined, "User has not joined this channel"}, St}
	end;

%% ---------------------------------------------------------------------------
% Sending messages
% Sends a given message in the current channel, sends the error "User has not joined this channel"
% if the user isn't in the current channel.
handle(St, {msg_from_GUI, Channel, Msg}) ->
	ChannelAtom = list_to_atom(Channel),
	Response = genserver:request(ChannelAtom, {send_msg, self(), St#client_st.nick, Msg}),
	case Response of
		ok ->
			{reply, ok, St};
		no_user_of_channel ->
			{reply, {error, user_not_joined, "User has not joined this channel"}, St}
	end;

%% ---------------------------------------------------------------------------
%% Get current nick
% Returns the current client nick.
handle(St, whoami) ->
    {reply, St#client_st.nick, St};

%% ---------------------------------------------------------------------------
%% Change nick
% Trying to change the nick and sends the error "You can not change nickname while connected to a server"
% if the client isn't connected to any server.
handle(St, {nick, Nick}) ->
	case St#client_st.connected of
		true ->
			{reply, {error, user_already_connected, "You can not change nickname while connected to a server"}, St};
		false ->
			{reply, ok, St#client_st{nick = Nick}}
	end;

%% ---------------------------------------------------------------------------
%% Incoming message
% Handles incoming messages
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.

%% ---------------------------------------------------------------------------
