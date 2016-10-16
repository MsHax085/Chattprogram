-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ChannelName) ->
  io:format("Channel `~p' created.", [ChannelName]),
  #channel_st{clients = dict:new(), channel = ChannelName}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.
%% Connect client

handle(St, {join_channel, Pid, Nick}) ->
	case dict:is_key(Pid, St#channel_st.clients) of
		false ->
			% Add client to channel
			io:fwrite("User ~p joined channel ~p.~n", [Nick, St#channel_st.channel]),
			{reply, ok, St#channel_st{clients = dict:store(Pid, Nick, St#channel_st.clients)}};
		true ->
			{reply, already_in_channel, St}
	end;
	
handle(St, {leave_channel, Pid, Nick}) ->
	case dict:is_key(Pid, St#channel_st.clients) of
		true ->
			% Remove client from channel
			{reply, ok, St#channel_st{clients = dict:erase(Pid, St#channel_st.clients)}};
		false ->
			{reply, no_user_of_channel, St}
	end;

handle(St, {send_msg, Pid, Nick, Msg}) ->
	case dict:is_key(Pid, St#channel_st.clients) of
		true ->
			% Send message to clients in channel
			% Credits: http://erlang.org/pipermail/erlang-questions/2011-April/057778.html
			io:fwrite("Process ~p tries to send a message.~n", [Pid]),
			[spawn(fun() -> genserver:request(X, {incoming_msg, St#channel_st.channel, Nick, Msg}) end) || X <- dict:fetch_keys(St#channel_st.clients), X /= Pid],
			{reply, ok, St};
		false ->
			{reply, no_user_of_channel, St}
	end.