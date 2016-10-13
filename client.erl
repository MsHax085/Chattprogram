-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

% http://learnyousomeerlang.com/a-short-visit-to-common-data-structures

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {gui = GUIName, nick = Nick, connected = false, serverRef = false}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
%    Data = "hello?",
%    io:fwrite("Client is sending: ~p~n", [Data]),
%    ServerAtom = list_to_atom(Server),
%    Response = genserver:request(ServerAtom, Data),
%    io:fwrite("Client received: ~p~n", [Response]),
%    % {reply, ok, St} ;
%    {reply, {error, not_implemented, "Not implemented"}, St} ;
	ServerAtom = list_to_atom(Server),
	Response = genserver:request(ServerAtom, {conn, St#client_st.nick}),
	case Response of
		ok ->
			ReplySt = St#client_st{connected = true, serverRef = ServerAtom},
			{reply, ok, ReplySt};
		failed ->
			{reply, {error, is_connected, "User already connected"}, St}
	end;
	

%% Disconnect from server
% Send disconnect request to server specified by the serverRef atom in
% record #client_st of client St (client name).
% If ok => Update record #client_st
handle(St, disconnect) ->
%    % {reply, ok, St} ;
%    {reply, {error, not_implemented, "Not implemented"}, St} ;
io:fwrite("server: ~p~n", [St#client_st.serverRef]),
	Response = genserver:request(St#client_st.serverRef, {disconnect, St#client_st.nick}),
	case Response of
		ok ->
			ReplySt = St#client_st{connected = false, serverRef = false},
			{reply, ok, ReplySt};
		_ ->
			{reply, {error, is_connected, "Failed to disconnect"}, St}
	end;

% Join channel
handle(St, {join, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

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
	ReplySt = St#client_st{nick = Nick},
    {reply, ok, ReplySt};
    %{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
