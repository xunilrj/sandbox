
//Define EPFD Implementation
class EPFD(epfdInit: Init[EPFD]) extends ComponentDefinition {

  //EPFD subscriptions
  val timer = requires[Timer];
  val pLink = requires[PerfectLink];
  val epfd = provides[EventuallyPerfectFailureDetector];

  // EPDF component state and initialization
  
  //configuration parameters
  val self = epfdInit match {case Init(s: Address) => s};
  val topology = cfg.getValue[List[Address]]("epfd.simulation.topology");
  val delta = cfg.getValue[Long]("epfd.simulation.delay");
  
  //mutable state
  var period = cfg.getValue[Long]("epfd.simulation.delay");
  var alive = Set(cfg.getValue[List[Address]]("epfd.simulation.topology"): _*);
  var suspected = Set[Address]();
  var seqnum = 0;

  def startTimer(delay: Long): Unit = {
    val scheduledTimeout = new ScheduleTimeout(period);
    scheduledTimeout.setTimeoutEvent(CheckTimeout(scheduledTimeout));
    trigger(scheduledTimeout -> timer);
  }

  //EPFD event handlers
  ctrl uponEvent {
    /*
    upon event h Init i do 
        alive := Π 
        suspected := ∅ 
        delay := ∆ 
        startTimer(delay)
    */
    case _: Start => handle {
        alive = Set(cfg.getValue[List[Address]]("epfd.simulation.topology"): _*);
        suspected = Set[Address]();
        period = delta;
        startTimer(period);
    }
  }

  timer uponEvent {
    /*
    upon event <Timeout> do 
        if alive intersection suspected != ∅ then 
            delay := delay + ∆ 
        for all p ∈ topology do
            if (p /∈ alive) ∧ (p /∈ suspected) then 
                suspected := suspected ∪ {p} 
                trigger < <>P, Suspect|p>
            else if (p ∈ alive)∧(p ∈ suspected) then 
                suspected := suspected \{p}
                trigger < <>P, Restore|p>
            trigger <pp2p, Send|p, [HeartbeatRequest]>
        alive := ∅
        startTimer(delay)
    */
    case CheckTimeout(_) => handle {
      if (!alive.intersect(suspected).isEmpty) {
        period = period + delta;
      }
      
      seqnum = seqnum + 1;
      
      for (p <- topology) {
        if (!alive.contains(p) && !suspected.contains(p)) {
            suspected = suspected + p;
            trigger (Suspect(p) -> epfd);
        } else if (alive.contains(p) && suspected.contains(p)) {
            suspected = suspected - p;
            trigger(Restore(p) -> epfd);
        }
        trigger(PL_Send(p, HeartbeatRequest(seqnum)) -> pLink);
      }
      alive = Set[Address]();
      startTimer(period);
    }
  }

  pLink uponEvent {
    /*
    upon event <pp2p, Deliver|p, [HeartbeatRequest]> do
        trigger <pp2p, Send|p, [HeartbeatReply]>
    */
    case PL_Deliver(src, HeartbeatRequest(seq)) => handle {
        seqnum = seqnum + 1;
        trigger(PL_Send(src, HeartbeatReply(seqnum)) -> pLink);
    }
    /*
    upon event <pp2p, Deliver|p, [HeartbeatReply]> do
        alive := alive ∪ {p}
    */
    case PL_Deliver(src, HeartbeatReply(seq)) => handle {
        alive = alive + src;
    }
  }
};


