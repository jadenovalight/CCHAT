-module(client).
-export([handle/2, initial_state/3]).

% Client state record (保持原框架的简单结构)
-record(client_st, {
    gui,      % atom of the GUI process
    nick,     % nick/username of the client
    server,   % atom of the chat server
    channels = []  % List of {ChannelName, ChannelPid} tuples for joined channels
}).

% Initializes a new client state with given parameters
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

% Handle channel join request
handle(St, {join, Channel}) ->
    ServerAtom = St#client_st.server,
    Nick = St#client_st.nick,
    ClientPid = self(),  % 使用客户端进程自己的 PID
    
    % Check if already joined to this channel
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        {Channel, _ChannelPid} ->
            {reply, {error, user_already_joined, "You have already joined this channel"}, St};
        false ->
            try
                % Request server to join channel
                case genserver:request(ServerAtom, {join, Channel, ClientPid, Nick}, 1000) of
                    {ok, ChannelPid} ->
                        % Successfully joined - update channel list
                        NewChannels = [{Channel, ChannelPid} | St#client_st.channels],
                        {reply, ok, St#client_st{channels = NewChannels}};
                    {error, user_already_joined} ->
                        {reply, {error, user_already_joined, "You have already joined this channel"}, St};
                    _ ->
                        {reply, {error, server_not_reached, "Server error"}, St}
                end
            catch
                _:_ ->
                    {reply, {error, server_not_reached, "Cannot reach server"}, St}
            end
    end;

% Handle channel leave request
handle(St, {leave, Channel}) ->
    % Check if client is in this channel
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        false ->
            {reply, {error, user_not_joined, "You are not in this channel"}, St};
        {Channel, ChannelPid} ->
            ClientPid = self(),
            % Remove channel from client's list
            NewChannels = lists:keydelete(Channel, 1, St#client_st.channels),
            
            % Attempt to notify channel of departure (ignore failures)
            try
                genserver:request(ChannelPid, {leave, ClientPid}, 1000)
            catch
                _:_ -> ok
            end,
            
            {reply, ok, St#client_st{channels = NewChannels}}
    end;

% Handle message sending to a channel
% 保持你原有的完整判断逻辑
handle(St, {message_send, Channel, Msg}) ->
    % Check if client is in the channel
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        false ->
            % Not in channel - check with server if channel exists
            ServerAtom = St#client_st.server,
            Nick = St#client_st.nick,
            ClientPid = self(),
            
            try
                % Attempt to send message through server
                Result = genserver:request(ServerAtom, {message, Channel, Nick, Msg, ClientPid}, 1000),
                case Result of
                    {error, user_not_joined} ->
                        % Server confirms channel exists but client isn't joined
                        {reply, {error, user_not_joined, "You are not in this channel"}, St};
                    {error, channel_not_found} ->
                        % Server reports channel doesn't exist
                        {reply, {error, server_not_reached, "Cannot reach server"}, St};
                    _ ->
                        % Generic error fallback
                        {reply, {error, user_not_joined, "You are not in this channel"}, St}
                end
            catch
                _:_ ->
                    % Unable to communicate with server
                    {reply, {error, server_not_reached, "Cannot reach server"}, St}
            end;
        {Channel, ChannelPid} ->
            % Already in channel - send message directly
            Nick = St#client_st.nick,
            ClientPid = self(),
            
            try
                case genserver:request(ChannelPid, {message, Nick, Msg, ClientPid}, 1000) of
                    ok ->
                        {reply, ok, St};
                    _ ->
                        {reply, ok, St}
                end
            catch
                _:_ ->
                    % Handle channel unreachable scenarios (e.g., server shutdown)
                    {reply, {error, server_not_reached, "Cannot reach server"}, St}
            end
    end;

% Handle nickname change request
handle(St, {nick, NewNick}) ->
    ServerAtom = St#client_st.server,
    OldNick = St#client_st.nick,
    
    % No change needed if new nickname matches current
    case NewNick of
        OldNick ->
            {reply, ok, St};
        _ ->
            try
                % Request server to change nickname
                case genserver:request(ServerAtom, {nick, OldNick, NewNick}, 1000) of
                    ok ->
                        {reply, ok, St#client_st{nick = NewNick}};
                    {error, nick_taken} ->
                        {reply, {error, nick_taken, "This nick is already taken"}, St};
                    _ ->
                        {reply, ok, St#client_st{nick = NewNick}}
                end
            catch
                _:_ ->
                    % On server communication failure, locally update nickname
                    {reply, ok, St#client_st{nick = NewNick}}
            end
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .