-module(broadcast).
-import(lists,[append/2]).
-export([start/0, add/2]).

start() -> spawn(fun() -> loop([]) end).
add(Pid, NewPid) -> Pid ! {add, NewPid}.

loop(Ps) -> 
    receive
        {add, Pid} ->
            Ps2 = append(Ps, [Pid]),
            loop(Ps2);
        {broadcast, From, Message} -> 
            io:format("Broadcast ~p ~p ~n", [From, Message]),
            lists:foreach(fun(X) -> X ! Message end, Ps),
            loop(Ps)
    end.
