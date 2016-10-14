-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

% http://learnyousomeerlang.com/a-short-visit-to-common-data-structures

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {gui = GUIName, nick = Nick, connected = false, serverRef = false, channels = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
	case St#client_st.connected of
		true ->
			{reply, {error, user_already_connected, "User already connected"}, St};
		false ->
			ServerAtom = list_to_atom(Server),
			Response = genserver:request(ServerAtom, {connect, self(), St#client_st.nick}),
			
			case Response of
				ok ->
					{reply, ok, St#client_st{connected = true, serverRef = ServerAtom}};
				_ ->
					{reply, {error, server_not_reached, "Server not reached"}, St}
			end
	end;
	
%% Disconnect from server
% Send disconnect request to server specified by the serverRef atom in
% record #client_st of client St (client name).
% If ok => Update record #client_st
handle(St, disconnect) ->
	case St#client_st.connected of
		true ->
			Response = genserver:request(St#client_st.serverRef, {disconnect, self(), St#client_st.nick}),
			case Response of
				ok ->
					{reply, ok, St#client_st{connected = false, serverRef = false}};
				leave_channels_first ->
					{reply, {error, leave_channels_first , "Leave all channels first"}, St};
				_ ->
					{reply, {error, server_not_reached  , "Server not reached"}, St}
			end;
		false ->
			{reply, {error, user_not_connected, "User not connected"}, St}
	end;

% Join channel
handle(St, {join, Channel}) ->
	case lists:keysearch(Channel, 1, St#client_st.channels) of
		false ->
			io:fwrite("Trying to connect ~p", [Channel]);
		_ ->
			{reply, {error, user_in_channel, "User is already connected to channel"}, St}
		end;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
	case St#client_st.connected of
		true ->
			{reply, {error, user_already_connected, "User already connected"}, St};
		false ->
			{reply, ok, St#client_st{nick = Nick}}
	end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
