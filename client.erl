-module(client).
-export([handle/2, initial_state/3]).

% Client state record
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
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        {Channel, _ChannelPid} ->
            {reply, {error, user_already_joined, "You have already joined this channel"}, St};
        false ->
            try
                case genserver:request(St#client_st.server, 
                                     {join, Channel, self(), St#client_st.nick}, 1000) of
                    {ok, ChannelPid} ->
                        NewChannels = [{Channel, ChannelPid} | St#client_st.channels],
                        {reply, ok, St#client_st{channels = NewChannels}};
                    {error, user_already_joined} ->
                        {reply, {error, user_already_joined, "You have already joined this channel"}, St};
                    _ ->
                        {reply, {error, server_not_reached, "Cannot reach server"}, St}
                end
            catch
                _:_ ->
                    {reply, {error, server_not_reached, "Cannot reach server"}, St}
            end
    end;

% Handle channel leave request
handle(St, {leave, Channel}) ->
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        false ->
            {reply, {error, user_not_joined, "You are not in this channel"}, St};
        {Channel, ChannelPid} ->
            NewChannels = lists:keydelete(Channel, 1, St#client_st.channels),
            try
                genserver:request(ChannelPid, {leave, self()}, 1000)
            catch
                _:_ -> ok
            end,
            {reply, ok, St#client_st{channels = NewChannels}}
    end;

% Handle message sending to a channel
handle(St, {message_send, Channel, Msg}) ->
    case lists:keyfind(Channel, 1, St#client_st.channels) of
        false ->
            % Not in channel - check with server if channel exists
            try
                Result = genserver:request(St#client_st.server, 
                                         {message, Channel, St#client_st.nick, Msg, self()}, 1000),
                case Result of
                    {error, user_not_joined} ->
                        {reply, {error, user_not_joined, "You are not in this channel"}, St};
                    {error, channel_not_found} ->
                        {reply, {error, server_not_reached, "Cannot reach server"}, St};
                    _ ->
                        {reply, {error, user_not_joined, "You are not in this channel"}, St}
                end
            catch
                _:_ ->
                    {reply, {error, server_not_reached, "Cannot reach server"}, St}
            end;
        {Channel, ChannelPid} ->
            % Already in channel - send message directly
            try
                case genserver:request(ChannelPid, {message, St#client_st.nick, Msg, self()}, 1000) of
                    ok ->
                        {reply, ok, St};
                    _ ->
                        {reply, ok, St}
                end
            catch
                _:_ ->
                    {reply, {error, server_not_reached, "Cannot reach server"}, St}
            end
    end;

% Handle nickname change request
handle(St, {nick, NewNick}) ->
    case NewNick of
        OldNick when OldNick =:= St#client_st.nick ->
            {reply, ok, St};
        _ ->
            try
                case genserver:request(St#client_st.server, 
                                     {nick, St#client_st.nick, NewNick}, 1000) of
                    ok ->
                        {reply, ok, St#client_st{nick = NewNick}};
                    {error, nick_taken} ->
                        {reply, {error, nick_taken, "This nick is already taken"}, St};
                    _ ->
                        {reply, ok, St#client_st{nick = NewNick}}
                end
            catch
                _:_ ->
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