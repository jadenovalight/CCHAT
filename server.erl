-module(server).
-export([start/1, stop/1]).

% Starts a new server process registered under the given atom
start(ServerAtom) ->
    % start(Atom, State, F)
    genserver:start(ServerAtom, [], fun handle/2).

% Stops the server and all associated channels
stop(ServerAtom) ->
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).

% State: List of channel names (strings)
% Handle join requests
handle(Channels, {join, Channel, ClientPid}) ->
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
            case genserver:request(list_to_atom(Channel), {message, Channel, Nick, Msg, ClientPid}) of
                ok ->
                    {reply, ok, Channels};
                {error, not_joined} ->
                    {reply, {error, user_not_joined}, Channels}
            end;
        false ->
            {reply, {error, channel_not_found}, Channels}
    end;

% Handle nickname change requests
handle(Channels, {nick, NewNick}) ->
    % Check if new nick is already in use in any channel
    IsTaken = lists:any(
        fun(Channel) ->
            case genserver:request(list_to_atom(Channel), {check_nick, NewNick}) of
                nick_taken -> true;
                ok -> false
            end
        end, Channels),
    
    case IsTaken of
        true ->
            {reply, {error, nick_taken}, Channels};
        false ->
            {reply, ok, Channels}
    end;

handle(Channels, stop_channels) ->
    % 遍历所有频道进程并停止它们
    % lists:foreach(匿名函数, 要遍历的列表)，fun ... end：定义匿名函数
    lists:foreach(
        fun(Channel) ->
            genserver:stop(list_to_atom(Channel))% list_to_atom(Channel)：将字符串转换为原子（进程注册名）
        end, Channels),
    {reply, ok, Channels}.

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
handle_channel(Clients, {message, Channel, Nick, Msg, SenderPid}) ->
    case lists:member(SenderPid, Clients) of
        true ->
            spawn(fun() ->
                lists:foreach(fun(ClientPid) ->
                    if 
                        ClientPid =/= SenderPid ->
                            genserver:request(ClientPid, {message_receive, Channel, Nick, Msg});
                        true -> 
                            ok
                    end
                end, Clients)
            end),
            {reply, ok, Clients};

        false ->
            {reply, {error, not_joined}, Clients}
    end;

% Check if a nick is used (for distinction assignment)
handle_channel(Clients, {check_nick, NewNick}) ->
    IsTaken = lists:any(
        fun(ClientPid) ->
            CurrentNick = genserver:request(ClientPid, whoami),
            CurrentNick =:= NewNick
        end, Clients),
    
    case IsTaken of
        true -> {reply, nick_taken, Clients};
        false -> {reply, ok, Clients}
    end.