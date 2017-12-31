-module(asktell).
-export([ask/2,ask_quorum/3]).

ask(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {response, Msg} -> Msg;
        _ -> ask(Pid, Message)
    end.

ask_quorum_internal(FAccept, NOk, NRej, Ayes, Nays) ->
    receive
        OriginalMessage ->
            case FAccept(OriginalMessage) of
                {ok, Message} -> 
                    NewAyes = lists:append(Ayes, [Message]),
                    NewNOk = NOk - 1,
                    if
                        NewNOk =< 0 -> {ok, NewAyes, Nays};
                        true -> ask_quorum_internal(FAccept, NewNOk, NRej, NewAyes, Nays)
                    end;
                {error, Message} ->
                    NewNays = lists:append(Nays, [Message]),
                    NewNRej = NRej - 1,
                    if
                        NewNRej =< 0 -> {error, Ayes, NewNays};
                        true -> ask_quorum_internal(FAccept, NOk, NewNRej, Ayes, NewNays)
                    end;
                Message -> ask_quorum_internal(FAccept, NOk, Rej, Ayes, Nays)
            end
    end.

ask_quorum(Pid, N, Message) ->
    Pid ! {self(), Message},
    %TODO Timeout
    ask_quorum_internal(fun(X) -> X end, (N/2)+1, (N/2)+1, [], []).
