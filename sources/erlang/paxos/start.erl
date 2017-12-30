-module(start).
-export([start/0]).

rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {response, Msg} -> Msg;
        _ -> rpc(Pid, Message)
    end.

start_paxos_node(Bid) ->
    PaxosIds = paxos:start(Bid),
    lists:foreach(fun(Pid) -> Bid ! {add,Pid} end, PaxosIds),
    PaxosIds.    

start() ->
    io:format("Start ~p ~n",[self()]),
    Bid = broadcast:start(),
    start_paxos_node(Bid),
    start_paxos_node(Bid),
    PaxosIds = start_paxos_node(Bid),
    BeforeValue = rpc(lists:nth(2, PaxosIds), {get}),
    lists:nth(3, PaxosIds) ! {propose, self(), 42},
    AfterValue = rpc(lists:nth(2, PaxosIds), {get}),
    io:format("Before ~p After ~p ~n",[BeforeValue, AfterValue]).