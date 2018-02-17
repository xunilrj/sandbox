
//Reliable Broadcast

class EagerReliableBroadcast(init: Init[EagerReliableBroadcast]) extends ComponentDefinition {
  //EagerReliableBroadcast Subscriptions
  val beb = requires[BestEffortBroadcast];
  val rb = provides[ReliableBroadcast];

  //EagerReliableBroadcast Component State and Initialization
  /*
  1: upon event <Init> do 
  2:    delivered := ∅ 
  */
  val self = init match {
    case Init(s: Address) => s
  };
  val delivered = collection.mutable.Set[KompicsEvent]();

  //EagerReliableBroadcast Event Handlers
  /*
  3: upon event <rb, Broadcast | m> do 
  4:    trigger <beb, Broadcast | [Data, self, m]>
  */
  rb uponEvent {
    case x @ RB_Broadcast(payload) => handle {
        trigger(BEB_Broadcast(x) -> beb);
    }
  }

  /*
  5: upon event <beb,Deliver | p, [Data,s,m]> do
  6:    if m /∈ delivered then 
  7:        delivered := delivered ∪ {m} 
  8:        trigger <rb, Deliver | s, m >
  9:        trigger <beb, Broadcast | [Data,s,m]>
  */
  beb uponEvent {
    case x @ BEB_Deliver(src, y @ RB_Broadcast(payload)) => handle {
        if(!delivered.contains(payload))
        {
            delivered += payload;
            trigger (RB_Deliver(src, payload) -> rb);
            trigger (BEB_Broadcast(y) -> beb);
        }
    }
  }
}