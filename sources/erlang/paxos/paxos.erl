-module(paxos).
-export([start/1]).

propose(Pid,Value) ->
    Pid ! {propose, Pid}.

start(BroadcastId) -> 
    Pid1 = spawn(fun() -> acceptor(BroadcastId, -1, -1) end),
    Pid2 = spawn(fun() -> learner(BroadcastId, -1) end),
    Pid3 = spawn(fun() -> proposer(BroadcastId, 0) end),
    [Pid1, Pid2, Pid3].

proposer_accept(BroadcastId, N, Acks) ->
    receive
        {ack, N, AcceptedValue} = Msg ->
            io:format("Proposer ~p ~p~n",[self(), Msg]),
            NewAcks = Acks + 1,
            if
                NewAcks >= 2 -> 
                    io:format("Proposer Accept Quorum achieved!~n"),
                    BroadcastId ! {broadcast, self(), {decide, self(), AcceptedValue}},
                    proposer(BroadcastId, N);
                true ->
                    proposer_accept(BroadcastId, N, NewAcks)
            end;            
        {nack, N, AcceptedValue} = Msg ->
            io:format("Proposer ~p ~p~n",[self(), Msg]),
            proposer_accept(BroadcastId, N, Acks);
        _ = Msg ->
            io:format("Proposer ~p [Ignored] ~p~n",[self(), Msg]),
            proposer_accept(BroadcastId, N, Acks)
    end.
proposer_prepare(BroadcastId, N, Acks, ProposedValue) ->
    receive
        {ack, N, AcceptedValue} = Msg ->
            io:format("Proposer ~p ~p~n",[self(), Msg]),
            NewAcks = Acks + 1,
            if
                NewAcks >= 2 -> 
                    io:format("Proposer Prepare Quorum achieved!~n"),
                    %TODO GET HIGHET VALUE
                    ProposedValue = if
                        AcceptedValue == -1 -> ProposedValue;
                        true -> AcceptedValue
                    end,
                    BroadcastId ! {broadcast, self(), {accept, self(), N, ProposedValue}},
                    proposer_accept(BroadcastId, N, 0);
                true ->
                    proposer_prepare(BroadcastId, N, NewAcks, ProposedValue)
            end;            
        {nack, N, AcceptedValue} = Msg ->
            io:format("Proposer ~p ~p~n",[self(), Msg]),
            proposer_prepare(BroadcastId, N, Acks, ProposedValue);
        _ = Msg ->
            io:format("Proposer ~p [Ignored] ~p~n",[self(), Msg]),
            proposer_prepare(BroadcastId, N, Acks, ProposedValue)
    end.
proposer(BroadcastId, N) ->
    receive
        {propose, Pid, Value} = Msg-> 
            io:format("Proposer ~p ~p~n",[self(), Msg]),
            NextN = N + 1,
            BroadcastId ! {broadcast, self(), {prepare, self(), NextN, Value}},
            proposer_prepare(BroadcastId, NextN, 0, Value);
        _ -> proposer(BroadcastId, N)
    end.

acceptor(BroadcastId, HighestPromisedN, AcceptedValue) ->
    receive
        {prepare, Source, N, Value} = Msg ->
            io:format("Acceptor ~p ~p~n",[self(), Msg]),
            if
                N > HighestPromisedN -> 
                    NewHighestPromisedN = HighestPromisedN,
                    Source ! {ack, N, AcceptedValue};
                true ->
                    NewHighestPromisedN = HighestPromisedN,
                    Source ! {nack, N, AcceptedValue}
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
            acceptor(BroadcastId, HighestPromisedN, NewValue)
    end.

learner(BroadcastId, Value) ->
    receive
        {Source, {get}} ->
            Source ! {response, Value},
            learner(BroadcastId, Value);
        {decide, Source, AcceptedValue} = Msg ->
            io:format("Learner ~p ~p~n",[self(), Msg]),
            learner(BroadcastId, AcceptedValue);
        _ -> learner(BroadcastId, Value)
    end.
