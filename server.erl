-module(server).
-export([start/1, stop/1]).

% Server state record
-record(server_st, {
    channels = [],  % List of {ChannelName, ChannelPid} tuples
    nicks = []      % List of registered nicknames
}).

% Channel state record
-record(channel_st, {
    name,           % Name of the channel
    clients = []    % List of ClientPid only (不存储昵称)
}).

% Starts a new server process registered under the given atom
start(ServerAtom) ->
    InitialState = #server_st{},
    genserver:start(ServerAtom, InitialState, fun handle/2).

% Stops the server and all associated channels
stop(ServerAtom) ->
    try
        State = genserver:request(ServerAtom, get_state, 1000),
        lists:foreach(fun({_Channel, ChannelPid}) ->
            ChannelPid ! stop
        end, State#server_st.channels)
    catch
        _:_ -> ok
    end,
    genserver:stop(ServerAtom),
    ok.

% Handle join requests
handle(State, {join, Channel, ClientPid, Nick}) ->
    % Add nickname to server registry if it's new
    NewNicks = case lists:member(Nick, State#server_st.nicks) of
        false -> [Nick | State#server_st.nicks];
        true -> State#server_st.nicks
    end,
    
    % Check if channel already exists
    case lists:keyfind(Channel, 1, State#server_st.channels) of
        {Channel, ChannelPid} ->
            % Join existing channel
            case genserver:request(ChannelPid, {join, ClientPid}) of
                ok ->
                    {reply, {ok, ChannelPid}, State#server_st{nicks = NewNicks}};
                {error, already_joined} ->
                    {reply, {error, user_already_joined}, State#server_st{nicks = NewNicks}}
            end;
        false ->
            % Create new channel
            ChannelState = #channel_st{
                name = Channel,
                clients = [ClientPid]  % 只存储 ClientPid
            },
            ChannelPid = spawn(fun() -> channel_loop(ChannelState) end),
            NewChannels = [{Channel, ChannelPid} | State#server_st.channels],
            {reply, {ok, ChannelPid}, State#server_st{channels = NewChannels, nicks = NewNicks}}
    end;

% Handle message requests (保持你的判断逻辑)
handle(State, {message, Channel, Nick, Msg, ClientPid}) ->
    case lists:keyfind(Channel, 1, State#server_st.channels) of
        {Channel, ChannelPid} ->
            case genserver:request(ChannelPid, {message, Nick, Msg, ClientPid}) of
                ok ->
                    {reply, ok, State};
                {error, not_joined} ->
                    {reply, {error, user_not_joined}, State}
            end;
        false ->
            {reply, {error, channel_not_found}, State}
    end;

% Handle nickname change requests
handle(State, {nick, OldNick, NewNick}) ->
    case lists:member(NewNick, State#server_st.nicks) of
        true ->
            {reply, {error, nick_taken}, State};
        false ->
            NewNicks = [NewNick | lists:delete(OldNick, State#server_st.nicks)],
            % 不需要通知频道更新昵称，因为频道不存储昵称
            {reply, ok, State#server_st{nicks = NewNicks}}
    end;

% Get state (for administrative purposes)
handle(State, get_state) ->
    {reply, State, State};

% Health check
handle(State, ping) ->
    {reply, pong, State};

% Unknown request
handle(State, _) ->
    {reply, {error, unknown_request}, State}.

% Channel loop - 使用 genserver 风格但简化版
% (如果要完全使用 genserver，可以改用 genserver:start，但这样也可以)
channel_loop(State) ->
    receive
        {request, From, Ref, Request} ->
            case Request of
                {join, ClientPid} ->
                    case lists:member(ClientPid, State#channel_st.clients) of
                        true ->
                            From ! {result, Ref, {error, already_joined}},
                            channel_loop(State);
                        false ->
                            NewClients = [ClientPid | State#channel_st.clients],
                            From ! {result, Ref, ok},
                            channel_loop(State#channel_st{clients = NewClients})
                    end;
                
                {leave, ClientPid} ->
                    case lists:member(ClientPid, State#channel_st.clients) of
                        true ->
                            NewClients = lists:delete(ClientPid, State#channel_st.clients),
                            From ! {result, Ref, ok},
                            channel_loop(State#channel_st{clients = NewClients});
                        false ->
                            From ! {result, Ref, {error, not_joined}},
                            channel_loop(State)
                    end;
                
                {message, Nick, Msg, SenderPid} ->
                    case lists:member(SenderPid, State#channel_st.clients) of
                        true ->
                            % Broadcast message to all clients except sender
                            lists:foreach(fun(ClientPid) ->
                                if 
                                    ClientPid =/= SenderPid ->
                                        ClientPid ! {request, self(), make_ref(), 
                                                    {message_receive, State#channel_st.name, Nick, Msg}};
                                    true -> ok
                                end
                            end, State#channel_st.clients),
                            From ! {result, Ref, ok},
                            channel_loop(State);
                        false ->
                            From ! {result, Ref, {error, not_joined}},
                            channel_loop(State)
                    end;
                
                _ ->
                    From ! {result, Ref, {error, unknown_request}},
                    channel_loop(State)
            end;
        
        stop ->
            ok
    end.