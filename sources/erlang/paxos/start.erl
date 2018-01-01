-module(start).
-import(asktell,[ask/2]).
-export([start/0]).

start_paxos_node(Bid) ->
    Pids = paxos:start(Bid),
    lists:foreach(fun(Pid) -> Bid ! {add,Pid} end, Pids),
    Pids.

start() ->
    io:format("Start ~p ~n",[self()]),
    Bid = broadcast:start(),
    start_paxos_node(Bid),
    start_paxos_node(Bid),
    Pids = start_paxos_node(Bid),
    paxos:propose(Pids, 42),
    BeforeValue = ask(lists:nth(2, Pids), {get}),
    lists:nth(3, Pids) ! {propose, self(), 42},
    AfterValue = ask(lists:nth(2, Pids), {get}),
    io:format("Before ~p After ~p ~n",[BeforeValue, AfterValue]).