-module(client).
-export([handle/2, initial_state/3]).

% Client state record
% Maintains client-specific information and connection status
-record(client_st, {
    gui,             % PID/Atom of the GUI process
    nick,            % Client's current nickname
    server,          % Atom of the connected server
    channels = [],   % List of {ChannelName, ChannelPid} tuples for joined channels
    receiver         % PID of the message receiver process
}).

% Initializes a new client state with given parameters
% Spawns a receiver process for handling incoming messages
initial_state(Nick, GUIAtom, ServerAtom) ->
    ReceiverPid = spawn(fun() -> receiver_loop(GUIAtom) end),
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = [],
        receiver = ReceiverPid
    }.

% Message receiver loop
% Handles incoming messages from channels and forwards to GUI
receiver_loop(GUI) ->
    receive
        {message_receive, Channel, Nick, Msg} ->
            gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
            receiver_loop(GUI);
        stop ->
            ok  % Terminate loop when stop signal received
    end.

% Handle channel join request
handle(St, {join, Channel}) ->
    ServerAtom = St#client_st.server,
    Nick = St#client_st.nick,
    ReceiverPid = St#client_st.receiver,
    
    % Check if already joined to this channel
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        {Channel, _ChannelPid} ->
            {reply, {error, user_already_joined, "You have already joined this channel"}, St};
        false ->
            try
                % Request server to join channel
                case genserver:request(ServerAtom, {join, Channel, ReceiverPid, Nick}, 1000) of
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
            ReceiverPid = St#client_st.receiver,
            % Remove channel from client's list
            NewChannels = lists:keydelete(Channel, 1, St#client_st.channels),
            
            % Attempt to notify channel of departure (ignore failures)
            try
                genserver:request(ChannelPid, {leave, ReceiverPid}, 1000)
            catch
                _:_ -> ok
            end,
            
            {reply, ok, St#client_st{channels = NewChannels}}
    end;

% Handle message sending to a channel
handle(St, {message_send, Channel, Msg}) ->
    % Check if client is in the channel
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        false ->
            % Not in channel - check with server if channel exists
            ServerAtom = St#client_st.server,
            Nick = St#client_st.nick,
            ReceiverPid = St#client_st.receiver,
            
            try
                % Attempt to send message through server
                Result = genserver:request(ServerAtom, {message, Channel, Nick, Msg, ReceiverPid}, 1000),
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
            ReceiverPid = St#client_st.receiver,
            
            try
                case genserver:request(ChannelPid, {message, Nick, Msg, ReceiverPid}, 1000) of
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

% Handle request for current nickname
handle(St, whoami) ->
    {reply, St#client_st.nick, St};

% Handle incoming message from channel (forward to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St};

% Handle client quit request
handle(St, quit) ->
    ReceiverPid = St#client_st.receiver,
    % Notify all joined channels of departure
    lists:foreach(fun({_Channel, ChannelPid}) ->
        try
            genserver:request(ChannelPid, {leave, ReceiverPid}, 500)
        catch
            _:_ -> ok  % Ignore failures during cleanup
        end
    end, St#client_st.channels),
    % Stop the receiver process
    ReceiverPid ! stop,
    {reply, ok, St};

% Catch-all handler for unimplemented commands
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.