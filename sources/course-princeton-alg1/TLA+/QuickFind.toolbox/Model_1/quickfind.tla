---------------------- MODULE quickfind ----------------------
EXTENDS Naturals, TLC, Sequences

CONSTANT N

(* --algorithm QuickFind {

variables
id = [j \in 0..N-1 |-> 0];
connected = [p1 \in 0..N-1, q1 \in 0..N-1 |-> FALSE]

procedure updateconnections()
variables i = 0; o = 0;
{
    updateconnection:while(i < N)
    {
        startinner: o := 0;
        updateinner:while(o < N)
        {
            connected[i,o] := id[i] = id[o];
            o := o + 1;
        };
        i := i + 1;
    };
    return;
}

procedure union(p, q)
variables i = 0; pid; qid;
{
    union1:pid := id[p];
    union2:qid := id[q];

    unionchange: while(i < N)
    {
        if (id[i] = pid)
        {
            id[i] := qid + 1;
        };
        i := i + 1;
    };
    
    unioncall1:call updateconnections();
    unionassert1:assert connected[p,q];
    unionassert2:assert connected[q,p];
    return;
}
    
process (Thread = 1)
variables i = 0; 
{
    init:while(i < N)
    {
        id[i] := i;
        i := i + 1;
    };
    union00:call union(0, 5);
    union01:call union(5, 6);
    
    assert1:assert connected[0,5];
    assert2:assert connected[5,0];
    assert3:assert connected[0,6];
    assert4:assert connected[6,0];
    assert5:assert connected[5,6];
    assert6:assert connected[6,5];
}

}*)
\* BEGIN TRANSLATION
\* Process variable i of process Thread at line 50 col 11 changed to i_
\* Procedure variable i of procedure updateconnections at line 13 col 11 changed to i_u
CONSTANT defaultInitValue
VARIABLES id, connected, pc, stack, i_u, o, p, q, i, pid, qid, i_

vars == << id, connected, pc, stack, i_u, o, p, q, i, pid, qid, i_ >>

ProcSet == {1}

Init == (* Global variables *)
        /\ id = [j \in 0..N-1 |-> 0]
        /\ connected = [p1 \in 0..N-1, q1 \in 0..N-1 |-> FALSE]
        (* Procedure updateconnections *)
        /\ i_u = [ self \in ProcSet |-> 0]
        /\ o = [ self \in ProcSet |-> 0]
        (* Procedure union *)
        /\ p = [ self \in ProcSet |-> defaultInitValue]
        /\ q = [ self \in ProcSet |-> defaultInitValue]
        /\ i = [ self \in ProcSet |-> 0]
        /\ pid = [ self \in ProcSet |-> defaultInitValue]
        /\ qid = [ self \in ProcSet |-> defaultInitValue]
        (* Process Thread *)
        /\ i_ = 0
        /\ stack = [self \in ProcSet |-> << >>]
        /\ pc = [self \in ProcSet |-> "init"]

updateconnection(self) == /\ pc[self] = "updateconnection"
                          /\ IF i_u[self] < N
                                THEN /\ pc' = [pc EXCEPT ![self] = "startinner"]
                                     /\ UNCHANGED << stack, i_u, o >>
                                ELSE /\ pc' = [pc EXCEPT ![self] = Head(stack[self]).pc]
                                     /\ i_u' = [i_u EXCEPT ![self] = Head(stack[self]).i_u]
                                     /\ o' = [o EXCEPT ![self] = Head(stack[self]).o]
                                     /\ stack' = [stack EXCEPT ![self] = Tail(stack[self])]
                          /\ UNCHANGED << id, connected, p, q, i, pid, qid, i_ >>

startinner(self) == /\ pc[self] = "startinner"
                    /\ o' = [o EXCEPT ![self] = 0]
                    /\ pc' = [pc EXCEPT ![self] = "updateinner"]
                    /\ UNCHANGED << id, connected, stack, i_u, p, q, i, pid, 
                                    qid, i_ >>

updateinner(self) == /\ pc[self] = "updateinner"
                     /\ IF o[self] < N
                           THEN /\ connected' = [connected EXCEPT ![i_u[self],o[self]] = id[i_u[self]] = id[o[self]]]
                                /\ o' = [o EXCEPT ![self] = o[self] + 1]
                                /\ pc' = [pc EXCEPT ![self] = "updateinner"]
                                /\ i_u' = i_u
                           ELSE /\ i_u' = [i_u EXCEPT ![self] = i_u[self] + 1]
                                /\ pc' = [pc EXCEPT ![self] = "updateconnection"]
                                /\ UNCHANGED << connected, o >>
                     /\ UNCHANGED << id, stack, p, q, i, pid, qid, i_ >>

updateconnections(self) == updateconnection(self) \/ startinner(self)
                              \/ updateinner(self)

union1(self) == /\ pc[self] = "union1"
                /\ pid' = [pid EXCEPT ![self] = id[p[self]]]
                /\ pc' = [pc EXCEPT ![self] = "union2"]
                /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, qid, i_ >>

union2(self) == /\ pc[self] = "union2"
                /\ qid' = [qid EXCEPT ![self] = id[q[self]]]
                /\ pc' = [pc EXCEPT ![self] = "unionchange"]
                /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, pid, i_ >>

unionchange(self) == /\ pc[self] = "unionchange"
                     /\ IF i[self] < N
                           THEN /\ IF id[i[self]] = pid[self]
                                      THEN /\ id' = [id EXCEPT ![i[self]] = qid[self] + 1]
                                      ELSE /\ TRUE
                                           /\ id' = id
                                /\ i' = [i EXCEPT ![self] = i[self] + 1]
                                /\ pc' = [pc EXCEPT ![self] = "unionchange"]
                           ELSE /\ pc' = [pc EXCEPT ![self] = "unioncall1"]
                                /\ UNCHANGED << id, i >>
                     /\ UNCHANGED << connected, stack, i_u, o, p, q, pid, qid, 
                                     i_ >>

unioncall1(self) == /\ pc[self] = "unioncall1"
                    /\ stack' = [stack EXCEPT ![self] = << [ procedure |->  "updateconnections",
                                                             pc        |->  "unionassert1",
                                                             i_u       |->  i_u[self],
                                                             o         |->  o[self] ] >>
                                                         \o stack[self]]
                    /\ i_u' = [i_u EXCEPT ![self] = 0]
                    /\ o' = [o EXCEPT ![self] = 0]
                    /\ pc' = [pc EXCEPT ![self] = "updateconnection"]
                    /\ UNCHANGED << id, connected, p, q, i, pid, qid, i_ >>

unionassert1(self) == /\ pc[self] = "unionassert1"
                      /\ Assert(connected[p[self],q[self]], 
                                "Failure of assertion at line 44, column 18.")
                      /\ pc' = [pc EXCEPT ![self] = "unionassert2"]
                      /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, 
                                      pid, qid, i_ >>

unionassert2(self) == /\ pc[self] = "unionassert2"
                      /\ Assert(connected[q[self],p[self]], 
                                "Failure of assertion at line 45, column 18.")
                      /\ pc' = [pc EXCEPT ![self] = Head(stack[self]).pc]
                      /\ i' = [i EXCEPT ![self] = Head(stack[self]).i]
                      /\ pid' = [pid EXCEPT ![self] = Head(stack[self]).pid]
                      /\ qid' = [qid EXCEPT ![self] = Head(stack[self]).qid]
                      /\ p' = [p EXCEPT ![self] = Head(stack[self]).p]
                      /\ q' = [q EXCEPT ![self] = Head(stack[self]).q]
                      /\ stack' = [stack EXCEPT ![self] = Tail(stack[self])]
                      /\ UNCHANGED << id, connected, i_u, o, i_ >>

union(self) == union1(self) \/ union2(self) \/ unionchange(self)
                  \/ unioncall1(self) \/ unionassert1(self)
                  \/ unionassert2(self)

init == /\ pc[1] = "init"
        /\ IF i_ < N
              THEN /\ id' = [id EXCEPT ![i_] = i_]
                   /\ i_' = i_ + 1
                   /\ pc' = [pc EXCEPT ![1] = "init"]
              ELSE /\ pc' = [pc EXCEPT ![1] = "union00"]
                   /\ UNCHANGED << id, i_ >>
        /\ UNCHANGED << connected, stack, i_u, o, p, q, i, pid, qid >>

union00 == /\ pc[1] = "union00"
           /\ /\ p' = [p EXCEPT ![1] = 0]
              /\ q' = [q EXCEPT ![1] = 5]
              /\ stack' = [stack EXCEPT ![1] = << [ procedure |->  "union",
                                                    pc        |->  "union01",
                                                    i         |->  i[1],
                                                    pid       |->  pid[1],
                                                    qid       |->  qid[1],
                                                    p         |->  p[1],
                                                    q         |->  q[1] ] >>
                                                \o stack[1]]
           /\ i' = [i EXCEPT ![1] = 0]
           /\ pid' = [pid EXCEPT ![1] = defaultInitValue]
           /\ qid' = [qid EXCEPT ![1] = defaultInitValue]
           /\ pc' = [pc EXCEPT ![1] = "union1"]
           /\ UNCHANGED << id, connected, i_u, o, i_ >>

union01 == /\ pc[1] = "union01"
           /\ /\ p' = [p EXCEPT ![1] = 5]
              /\ q' = [q EXCEPT ![1] = 6]
              /\ stack' = [stack EXCEPT ![1] = << [ procedure |->  "union",
                                                    pc        |->  "assert1",
                                                    i         |->  i[1],
                                                    pid       |->  pid[1],
                                                    qid       |->  qid[1],
                                                    p         |->  p[1],
                                                    q         |->  q[1] ] >>
                                                \o stack[1]]
           /\ i' = [i EXCEPT ![1] = 0]
           /\ pid' = [pid EXCEPT ![1] = defaultInitValue]
           /\ qid' = [qid EXCEPT ![1] = defaultInitValue]
           /\ pc' = [pc EXCEPT ![1] = "union1"]
           /\ UNCHANGED << id, connected, i_u, o, i_ >>

assert1 == /\ pc[1] = "assert1"
           /\ Assert(connected[0,5], 
                     "Failure of assertion at line 60, column 13.")
           /\ pc' = [pc EXCEPT ![1] = "assert2"]
           /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, pid, qid, i_ >>

assert2 == /\ pc[1] = "assert2"
           /\ Assert(connected[5,0], 
                     "Failure of assertion at line 61, column 13.")
           /\ pc' = [pc EXCEPT ![1] = "assert3"]
           /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, pid, qid, i_ >>

assert3 == /\ pc[1] = "assert3"
           /\ Assert(connected[0,6], 
                     "Failure of assertion at line 62, column 13.")
           /\ pc' = [pc EXCEPT ![1] = "assert4"]
           /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, pid, qid, i_ >>

assert4 == /\ pc[1] = "assert4"
           /\ Assert(connected[6,0], 
                     "Failure of assertion at line 63, column 13.")
           /\ pc' = [pc EXCEPT ![1] = "assert5"]
           /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, pid, qid, i_ >>

assert5 == /\ pc[1] = "assert5"
           /\ Assert(connected[5,6], 
                     "Failure of assertion at line 64, column 13.")
           /\ pc' = [pc EXCEPT ![1] = "assert6"]
           /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, pid, qid, i_ >>

assert6 == /\ pc[1] = "assert6"
           /\ Assert(connected[6,5], 
                     "Failure of assertion at line 65, column 13.")
           /\ pc' = [pc EXCEPT ![1] = "Done"]
           /\ UNCHANGED << id, connected, stack, i_u, o, p, q, i, pid, qid, i_ >>

Thread == init \/ union00 \/ union01 \/ assert1 \/ assert2 \/ assert3
             \/ assert4 \/ assert5 \/ assert6

Next == Thread
           \/ (\E self \in ProcSet: updateconnections(self) \/ union(self))
           \/ (* Disjunct to prevent deadlock on termination *)
              ((\A self \in ProcSet: pc[self] = "Done") /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION
===================================================================
