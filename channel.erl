-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% ---------------------------------------------------------------------------
%% Produce initial state
% Creates a record with dict clients and a state with the channel name.
initial_state(ChannelName) ->
  io:format("Channel `~p' created.", [ChannelName]),
  #channel_st{clients = dict:new(), channel = ChannelName}.

%% ---------------------------------------------------------------------------
%% Join channel
% Adds the client to the given channel and sends error "already_in_channel" if
% the client already is connected.
handle(St, {join_channel, Pid, Nick}) ->
	case dict:is_key(Pid, St#channel_st.clients) of
		false ->
			% Add client to channel
			io:fwrite("User ~p joined channel ~p.~n", [Nick, St#channel_st.channel]),
			{reply, ok, St#channel_st{clients = dict:store(Pid, Nick, St#channel_st.clients)}};
		true ->
			{reply, already_in_channel, St}
	end;
	
%% ---------------------------------------------------------------------------
%% Leave channel
% Removes the given Pid & Nick from the channel and sends error
% "no_user_of_channel" if the given user isn't in the channel dict.
handle(St, {leave_channel, Pid}) ->
	case dict:is_key(Pid, St#channel_st.clients) of
		true ->
			% Remove client from channel
			{reply, ok, St#channel_st{clients = dict:erase(Pid, St#channel_st.clients)}};
		false ->
			{reply, no_user_of_channel, St}
	end;

%% ---------------------------------------------------------------------------
%% Send message
% Sends message to all users in the channel that is not equal to the given Pid & Nick.
handle(St, {send_msg, Pid, Nick, Msg}) ->
	case dict:is_key(Pid, St#channel_st.clients) of
		true ->
			% Send message to clients in channel
			io:fwrite("Process ~p tries to send a message.~n", [Pid]),
			[spawn(fun() -> genserver:request(X, {incoming_msg, St#channel_st.channel, Nick, Msg}) end) || X <- dict:fetch_keys(St#channel_st.clients), X /= Pid],
			{reply, ok, St};
		false ->
			{reply, no_user_of_channel, St}
	end.

%% ---------------------------------------------------------------------------
