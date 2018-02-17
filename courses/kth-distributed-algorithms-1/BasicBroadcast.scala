class BasicBroadcast(init: Init[BasicBroadcast]) extends ComponentDefinition {

  //BasicBroadcast Subscriptions
  val pLink = requires[PerfectLink];
  val beb = provides[BestEffortBroadcast];

  //BasicBroadcast Component State and Initialization
  val (self, topology) = init match {
    case Init(s: Address, t: Set[Address]@unchecked) => (s, t)
  };

  //BasicBroadcast Event Handlers
  /*
  1: upon event <beb, Broadcast | m> do
  2:    for all q ∈ Π do
  3:        trigger <pp2p, Send | q,m> 
  */
  beb uponEvent {
    case x: BEB_Broadcast => handle {
        for(q <- topology)
            trigger(PL_Send(q, x) -> pLink);
    }
  }

  /*
  4: upon event <pp2p, Deliver | p, m> do
  5:    trigger <beb, Deliver | p, m>
  */
  pLink uponEvent {
    case PL_Deliver(src, BEB_Broadcast(payload)) => handle {
        trigger(BEB_Deliver(src, payload) -> beb);
    }
  }
}