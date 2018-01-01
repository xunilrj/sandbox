-module(paxos).
-import(asktell, [ask_quorum/3]).
-export([start/1, propose/2]).

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).
print(Role, Obj1) ->
    io:format("~p ~p ~p ~p~n",[get_timestamp(), self(), Role, Obj1]).
print(Role, Obj1, Obj2) ->
    io:format("~p ~p ~p ~p ~p~n",[get_timestamp(), self(), Role, Obj1, Obj2]).

propose(Pids, Value) ->
    lists:nth(3,Pids) ! {propose, self(), Value}.

start(BroadcastId) -> 
    Pid1 = spawn(fun() -> acceptor(BroadcastId, -1, -1) end),
    Pid2 = spawn(fun() -> learner(BroadcastId, -1) end),
    Pid3 = spawn(fun() -> proposer(BroadcastId, 0) end),
    [Pid1, Pid2, Pid3].

proposer(BroadcastId, N) ->
    receive
        {propose, Pid, Value} = Msg-> 
            receive _ -> io:format("x") end,
            print(?FUNCTION_NAME, Msg),
            NextN = N + 1,
            case ask_quorum(BroadcastId, 3, {prepare, NextN}) of                
                {error, _, _} ->
                    Pid ! {error, Value},
                    proposer(BroadcastId, NextN);
                {ok, Accepteds, Rejecteds} ->
                    io:format("Proposer Prepare Quorum!"),
                    % FIND HIGHEST
                    {PreparedN, PreparedValue} = lists:nth(1, Accepteds),
                    case ask_quorum(BroadcastId, 3, {accept, PreparedN, PreparedValue}) of                
                        {error, Accepteds, Rejecteds} -> 
                            Pid ! {error, Value},
                            proposer(BroadcastId, N);
                        {ok, Accepteds, Rejecteds} ->
                            io:format("Proposer Accept Quorum!"),
                            %TODO Find Highest?
                            {_, AcceptedValue} = lists:nth(1, Accepteds),
                            Pid ! {ok, AcceptedValue}
                    end                    
            end;      
        _ -> proposer(BroadcastId, N)
    end.

acceptor(BroadcastId, HighestPromisedN, AcceptedValue) ->
    receive
        {Pid, {prepare, N} = Msg} ->
            io:format("Acceptor ~p ~p~n",[self(), Msg]),
            if
                N > HighestPromisedN -> 
                    NewHighestPromisedN = HighestPromisedN,
                    Pid ! {ack, N, AcceptedValue};
                true ->
                    NewHighestPromisedN = HighestPromisedN,
                    Pid ! {nack, N, AcceptedValue}
            end,
            acceptor(BroadcastId, NewHighestPromisedN, AcceptedValue);
        {accept, Source, N, Value} = Msg ->
            io:format("Acceptor ~p ~p~n",[self(), Msg]),
            if
                N > HighestPromisedN -> 
                    NewValue = Value,
                    Source ! {ack, N, NewValue};
                true ->    
                    NewValue = AcceptedValue,                
                    Source ! {nack, N, AcceptedValue}
            end,
            acceptor(BroadcastId, HighestPromisedN, NewValue);
        Message -> 
            print(?FUNCTION_NAME, "[Ignored]", Message)
    end.

learner(BroadcastId, Value) ->
    receive
        {Source, {get}} ->
            Source ! {response, Value},
            learner(BroadcastId, Value);
        {decide, _, AcceptedValue} = Msg ->
            io:format("Learner ~p ~p~n",[self(), Msg]),
            learner(BroadcastId, AcceptedValue);
        _ -> learner(BroadcastId, Value)
    end.
