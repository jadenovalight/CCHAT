-module(server).
-export([start/1,stop/1]).

% Server state record
% Contains lists of existing channels and registered nicknames
-record(server_st, {
    channels = [],  % List of {ChannelName, ChannelPid} tuples
    nicks = []      % List of registered nicknames
}).

% Channel state record
% Maintains information about a specific chat channel
-record(channel_st, {
    name,           % Name of the channel
    clients = [],   % List of {ClientPid, Nickname} tuples
    server_pid      % PID of the server managing this channel
}).

% Starts a new server process registered under the given atom
% Returns the PID of the newly created server
start(ServerAtom) ->
    InitialState = #server_st{},
    ServerPid = genserver:start(ServerAtom, InitialState, fun handle_server/2),
    ServerPid.

% Stops the server and all associated channels
stop(ServerAtom) ->
    % Retrieve server state and stop all channels
    try
        State = genserver:request(ServerAtom, get_state, 1000),
        lists:foreach(fun({_Channel, ChannelPid}) ->
            ChannelPid ! stop
        end, State#server_st.channels)
    catch
        _:_ -> ok  % Ignore errors if server is already stopping
    end,
    genserver:stop(ServerAtom),
    ok.

% Main server request handler
% Dispatches to appropriate handlers based on request type
handle_server(State, Request) ->
    case Request of
        {join, Channel, ClientPid, Nick} ->
            handle_join(State, Channel, ClientPid, Nick);
        
        {leave, Channel, ClientPid} ->
            handle_leave(State, Channel, ClientPid);
        
        {message, Channel, Nick, Msg, ClientPid} ->
            handle_message(State, Channel, Nick, Msg, ClientPid);
        
        {nick, OldNick, NewNick} ->
            handle_nick_change(State, OldNick, NewNick);
        
        get_state ->
            {reply, State, State};  % Return current state for administrative purposes
        
        ping ->
            {reply, pong, State};   % Simple health check
        
        _ ->
            {reply, {error, unknown_request}, State}
    end.

% Handles client join requests to a channel
% Creates new channel if it doesn't exist, registers nickname if new
handle_join(State, Channel, ClientPid, Nick) ->
    % Add nickname to server registry if it's new
    NewNicks = case lists:member(Nick, State#server_st.nicks) of
        false -> [Nick | State#server_st.nicks];
        true -> State#server_st.nicks
    end,
    
    % Check if channel already exists
    case find_channel(Channel, State#server_st.channels) of
        {ok, ChannelPid} ->
            % Join existing channel
            case genserver:request(ChannelPid, {join, ClientPid, Nick}) of
                ok ->
                    {reply, {ok, ChannelPid}, State#server_st{nicks = NewNicks}};
                {error, already_joined} ->
                    {reply, {error, user_already_joined}, State#server_st{nicks = NewNicks}}
            end;
        not_found ->
            % Create new channel if it doesn't exist
            ServerPid = self(),
            ChannelState = #channel_st{
                name = Channel, 
                clients = [{ClientPid, Nick}],  % Add initial client
                server_pid = ServerPid
            },
            % Spawn new channel process with error trapping
            ChannelPid = spawn(fun() -> 
                process_flag(trap_exit, true),
                link(ServerPid),  % Link to server for process monitoring
                channel_loop(ChannelState) 
            end),
            NewChannels = [{Channel, ChannelPid} | State#server_st.channels],
            {reply, {ok, ChannelPid}, State#server_st{channels = NewChannels, nicks = NewNicks}}
    end.

% Handles client leave requests from a channel
handle_leave(State, Channel, ClientPid) ->
    case find_channel(Channel, State#server_st.channels) of
        {ok, ChannelPid} ->
            % Attempt to remove client from channel
            case genserver:request(ChannelPid, {leave, ClientPid}) of
                ok ->
                    {reply, ok, State};
                {error, not_joined} ->
                    {reply, {error, user_not_joined}, State}
            end;
        not_found ->
            {reply, {error, user_not_joined}, State}
    end.

% Handles message broadcasting to a channel
handle_message(State, Channel, Nick, Msg, ClientPid) ->
    case find_channel(Channel, State#server_st.channels) of
        {ok, ChannelPid} ->
            % Send message to channel for broadcasting
            case genserver:request(ChannelPid, {message, Nick, Msg, ClientPid}) of
                ok ->
                    {reply, ok, State};
                {error, not_joined} ->
                    {reply, {error, user_not_joined}, State}
            end;
        not_found ->
            {reply, {error, channel_not_found}, State}
    end.

% Handles nickname change requests
handle_nick_change(State, OldNick, NewNick) ->
    % Check if new nickname is available
    case lists:member(NewNick, State#server_st.nicks) of
        true ->
            {reply, {error, nick_taken}, State};
        false ->
            % Update nickname registry
            NewNicks = [NewNick | lists:delete(OldNick, State#server_st.nicks)],
            % Notify all channels about nickname change
            lists:foreach(fun({_, ChannelPid}) ->
                ChannelPid ! {update_nick, OldNick, NewNick}
            end, State#server_st.channels),
            {reply, ok, State#server_st{nicks = NewNicks}}
    end.

% Helper function to find a channel's PID by name
find_channel(Channel, Channels) ->
    case lists:keyfind(Channel, 1, Channels) of
        {Channel, Pid} -> {ok, Pid};
        false -> not_found
    end.

% Main loop for channel process handling
% Manages channel state and processes requests
channel_loop(State) ->
    receive
        % Handle server process exit (keep channel running for offline messaging)
        {'EXIT', ServerPid, _Reason} when ServerPid == State#channel_st.server_pid ->
            channel_loop(State);
        
        % Handle requests from server
        {request, From, Ref, Request} ->
            case Request of
                {join, ClientPid, Nick} ->
                    % Check if client is already in channel
                    case is_member(ClientPid, State#channel_st.clients) of
                        true ->
                            From ! {result, Ref, {error, already_joined}},
                            channel_loop(State);
                        false ->
                            NewClients = [{ClientPid, Nick} | State#channel_st.clients],
                            From ! {result, Ref, ok},
                            channel_loop(State#channel_st{clients = NewClients})
                    end;
                
                {leave, ClientPid} ->
                    % Remove client from channel if present
                    case is_member(ClientPid, State#channel_st.clients) of
                        true ->
                            NewClients = remove_client(ClientPid, State#channel_st.clients),
                            From ! {result, Ref, ok},
                            channel_loop(State#channel_st{clients = NewClients});
                        false ->
                            From ! {result, Ref, {error, not_joined}},
                            channel_loop(State)
                    end;
                
                {message, Nick, Msg, SenderPid} ->
                    % Broadcast message to all channel members except sender
                    case is_member(SenderPid, State#channel_st.clients) of
                        true ->
                            broadcast_message(State#channel_st.name, Nick, Msg, 
                                            SenderPid, State#channel_st.clients),
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
        
        % Update client nickname in channel
        {update_nick, OldNick, NewNick} ->
            NewClients = update_nick_in_clients(OldNick, NewNick, State#channel_st.clients),
            channel_loop(State#channel_st{clients = NewClients});
        
        % Stop channel process
        stop ->
            ok
    end.

% Check if client is a member of the channel
is_member(ClientPid, Clients) ->
    lists:keymember(ClientPid, 1, Clients).

% Remove client from channel member list
remove_client(ClientPid, Clients) ->
    lists:keydelete(ClientPid, 1, Clients).

% Send message to all clients in channel except the sender
broadcast_message(Channel, Nick, Msg, SenderPid, Clients) ->
    lists:foreach(fun({ClientPid, _ClientNick}) ->
        if 
            ClientPid =/= SenderPid ->
                ClientPid ! {message_receive, Channel, Nick, Msg};
            true ->
                ok  % Don't send message back to sender
        end
    end, Clients).

% Update nickname for a client across all channel memberships
update_nick_in_clients(_OldNick, _NewNick, []) ->
    [];
update_nick_in_clients(OldNick, NewNick, [{Pid, OldNick} | Rest]) ->
    [{Pid, NewNick} | update_nick_in_clients(OldNick, NewNick, Rest)];
update_nick_in_clients(OldNick, NewNick, [Client | Rest]) ->
    [Client | update_nick_in_clients(OldNick, NewNick, Rest)].