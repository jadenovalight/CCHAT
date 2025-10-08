-module(server).
-export([start/1, stop/1]).

% State: List of channel names (strings)

% Starts a new server process registered under the given atom
start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun handle/2).

% Stops the server and all associated channels
stop(ServerAtom) ->
    try
        Channels = genserver:request(ServerAtom, get_state, 1000),
        lists:foreach(fun(Channel) ->
            genserver:stop(list_to_atom(Channel))
        end, Channels)
    catch
        _:_ -> ok
    end,
    genserver:stop(ServerAtom),
    ok.

% Handle join requests
handle(Channels, {join, Channel, ClientPid, _Nick}) ->
    case lists:member(Channel, Channels) of
        true ->
            % Channel exists, join it
            case genserver:request(list_to_atom(Channel), {join, ClientPid}) of
                ok ->
                    {reply, {ok, list_to_atom(Channel)}, Channels};
                {error, already_joined} ->
                    {reply, {error, user_already_joined}, Channels}
            end;
        false ->
            % Create new channel using genserver
            genserver:start(list_to_atom(Channel), [ClientPid], fun handle_channel/2),
            NewChannels = [Channel | Channels],
            {reply, {ok, list_to_atom(Channel)}, NewChannels}
    end;

% Handle message requests
handle(Channels, {message, Channel, Nick, Msg, ClientPid}) ->
    case lists:member(Channel, Channels) of
        true ->
            case genserver:request(list_to_atom(Channel), {message, Nick, Msg, ClientPid}) of
                ok ->
                    {reply, ok, Channels};
                {error, not_joined} ->
                    {reply, {error, user_not_joined}, Channels}
            end;
        false ->
            {reply, {error, channel_not_found}, Channels}
    end;

% Handle nickname change requests
handle(Channels, {nick, _OldNick, NewNick}) ->
    % Check if new nick is already in use in any channel
    IsTaken = lists:any(fun(Channel) ->
        try
            case genserver:request(list_to_atom(Channel), {check_nick, NewNick}, 1000) of
                nick_taken -> true;
                ok -> false
            end
        catch
            _:_ -> false
        end
    end, Channels),
    
    case IsTaken of
        true ->
            {reply, {error, nick_taken}, Channels};
        false ->
            {reply, ok, Channels}
    end;

% Get state (for administrative purposes)
handle(Channels, get_state) ->
    {reply, Channels, Channels};

% Health check
handle(Channels, ping) ->
    {reply, pong, Channels};

% Unknown request
handle(Channels, _) ->
    {reply, {error, unknown_request}, Channels}.


% ============================================================================
% Channel handler function
% ============================================================================

% Handle join request
handle_channel(Clients, {join, ClientPid}) ->
    case lists:member(ClientPid, Clients) of
        true ->
            {reply, {error, already_joined}, Clients};
        false ->
            NewClients = [ClientPid | Clients],
            {reply, ok, NewClients}
    end;

% Handle leave request
handle_channel(Clients, {leave, ClientPid}) ->
    case lists:member(ClientPid, Clients) of
        true ->
            NewClients = lists:delete(ClientPid, Clients),
            {reply, ok, NewClients};
        false ->
            {reply, {error, not_joined}, Clients}
    end;

% Handle message broadcast
handle_channel(Clients, {message, Nick, Msg, SenderPid}) ->
    case lists:member(SenderPid, Clients) of
        true ->
            ChannelAtom = process_info(self(), registered_name),
            ChannelName = case ChannelAtom of
                {registered_name, Name} -> 
                    atom_to_list(Name);
                _ -> 
                    "unknown"
            end,
            
            spawn(fun() ->
                lists:foreach(fun(ClientPid) ->
                    if 
                        ClientPid =/= SenderPid ->
                            try
                                genserver:request(ClientPid, {message_receive, ChannelName, Nick, Msg}, 1000)
                            catch
                                _:_ -> ok  % Ignore if client is unreachable
                            end;
                        true -> 
                            ok
                    end
                end, Clients)
            end),
            {reply, ok, Clients};

        false ->
            {reply, {error, not_joined}, Clients}
    end;

% Check if a nick is in use in this channel (for distinction assignment)
handle_channel(Clients, {check_nick, NewNick}) ->
    IsTaken = lists:any(fun(ClientPid) ->
        try
            CurrentNick = genserver:request(ClientPid, whoami, 1000),
            CurrentNick =:= NewNick
        catch
            _:_ -> false
        end
    end, Clients),
    
    case IsTaken of
        true -> {reply, nick_taken, Clients};
        false -> {reply, ok, Clients}
    end;

% Unknown request
handle_channel(Clients, _) ->
    {reply, {error, unknown_request}, Clients}.