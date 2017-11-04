

//Causal Reliable Broadcast

case class DataMessage(timestamp: VectorClock, payload: KompicsEvent) extends KompicsEvent;

class WaitingCRB(init: Init[WaitingCRB]) extends ComponentDefinition {

  //WaitingCRB Subscriptions
  val rb = requires[ReliableBroadcast];
  val crb = provides[CausalOrderReliableBroadcast];

  
  //WaitingCRB Component State and Initialization
  val (self, vec) = init match {
    case Init(s: Address, t: Set[Address]@unchecked) => (s, VectorClock.empty(t.toSeq))
  };

  //  val V = VectorClock.empty(init match { case Init(_, t: Set[Address]) => t.toSeq })
  var pending: ListBuffer[(Address, DataMessage)] = ListBuffer();
  var lsn = 0;
  
  //WaitingCRB Event Handlers
  /*
  5: upon event <crb,Broadcast | m> do 
  6:    W := V 
  7:    W[self ] := lsn 
  8:    lsn := lsn + 1 
  9:    trigger <rb,Broadcast | [Data,W,m]> 
  */
  crb uponEvent {
    case x: CRB_Broadcast => handle {
        var W = VectorClock(vec);
        W.set(self, lsn);
        lsn += 1;
        trigger(RB_Broadcast(DataMessage(W,x)) -> rb);
    }
  }

  /*
  10: upon event <rb,Deliver | p,[Data,W,m]> do
  11:   pending := pending ∪ {(p,W,m)}
  12:   while (∃(p0,W0,m0) ∈ pending) W0 ≤ V do
  13:       pending := pending \ {(p0,W0,m0)}
  14:       V[p0] := V[p0] + 1
  15:       trigger <crb,Deliver | p0,m0> 
  */
  rb uponEvent {
    case x @ RB_Deliver(src: Address, msg: DataMessage) => handle {
        pending += ((src, msg));
        val sortedPending = pending.sortWith(_._2.timestamp <= _._2.timestamp)
        for { 
            item0 @ (p0, DataMessage(w0, CRB_Broadcast(m0))) <- sortedPending if w0 <= vec
        } {
            pending -= item0;
            vec.inc(p0);
            trigger (CRB_Deliver(p0, m0) -> crb);
        }
    }
  }
}
