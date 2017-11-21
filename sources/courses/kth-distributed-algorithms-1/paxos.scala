class Paxos(paxosInit: Init[Paxos]) extends ComponentDefinition {

  //Port Subscriptions for Paxos

  val consensus = provides[Consensus];
  val beb = requires[BestEffortBroadcast];
  val plink = requires[PerfectLink];
 
  //Internal State of Paxos
  val (self, rank, numProcesses) = paxosInit match {
    case Init(s: Address, qSize: Int) => (s, toRank(s), qSize)
  }

  //Proposer State
  var round = 0;
  var proposedValue: Option[Any] = None;
  var promises: ListBuffer[((Int, Int), Option[Any])] = ListBuffer.empty;
  var numOfAccepts = 0;
  var decided = false;

  //Acceptor State
  var promisedBallot = (0, 0);
  var acceptedBallot = (0, 0);
  var acceptedValue: Option[Any] = None;

  /*
  8:function propose
  9: if ¬decided then
 10:   ts := ts + 1
 11:   numOfAccepts := 0
 12:   promises := ∅
 13:   trigger <beb, Broadcast|[Prepare,(ts,rank(self))]〉
  */
  def propose() = {
   if(decided == false){
       println(s"$self - not decided - proposing...");
       round = round + 1;
       numOfAccepts = 0;
       promises = ListBuffer.empty;
       trigger(BEB_Broadcast( Prepare( (round, rank) ) ) -> beb);
   } else {
       println(s"$self - already decided")
   }
  }
  
  consensus uponEvent {
    /*
    14:upon event〈c,Propose|v〉do
    15:  pv:=v
    16:  Propose()
    */
    case C_Propose(value) => handle {
        println(s"$self C_Propose($value)");
        proposedValue = Some(value)
        propose();
    }
  }


  beb uponEvent {
    /*
    29:upon event〈beb,Deliver|p,[Prepare, ballot]〉do
    30:     if promBallot < ballot then
    31:         promBallot := ballot
    32:         trigger (pp2p,Send|p, [Promise,promBallot,accBallot,av])
    33:     else
    34:         trigger(pp2p,Send|p, [Nack, ballot])
    */
    case BEB_Deliver(src, prep @ Prepare(proposalBallot) ) => handle {
        if(promisedBallot < proposalBallot) {
            println(s"$src -> $self - $promisedBallot - promising until $prep");
            promisedBallot = proposalBallot;
            trigger (PL_Send(src, Promise(promisedBallot, acceptedBallot, acceptedValue)) -> plink)
        } else {
            println(s"$src -> $self - $proposalBallot - ignoring because I am promised until $promisedBallot");
            trigger (PL_Send(src, Nack(proposalBallot)) -> plink)
        }
    };

    /*
    35:upon event〈beb,Deliver|p,[Accept, ballot, v]〉do
    36:     if promBallot ≤ ballot then
    37:         promBallot := accBallot := ballot
    38:         av := v
    39:         trigger〈pp2p,Send|p, [Accepted,ballot]〉
    40:     else
    41:         trigger〈pp2p,Send|p, [Nack,ballot]〉
    */
    case BEB_Deliver(src, acc @ Accept(acceptBallot, proposedValue)) => handle {
        if(promisedBallot <= acceptBallot){
            println(s"$src -> $self - $promisedBallot - $acc - accepted...");
            acceptedBallot = acceptBallot;
            promisedBallot = acceptBallot;
            acceptedValue = Some(proposedValue);
            trigger(PL_Send(src, Accepted(acceptBallot)) -> plink);
        } else {
            println(s"$src -> $self - just nacking...");
            trigger(PL_Send(src, Nack(acceptBallot)) -> plink);
        }
    };

    /*
    45:upon event〈beb,Deliver|p,[Decided, v]〉do
    46:     if ¬decided then
    47:         trigger〈c,Decide|v〉
    48:         decided:=true
    */
    case BEB_Deliver(src, dec @ Decided(decidedValue) ) => handle {
        if(decided == false) {
            trigger(C_Decide(decidedValue) -> consensus)
            decided = true
        }
    }
  }

  plink uponEvent {

    /*
    17:upon event〈pp2p,Deliver|p,[Promise, b, a, v]〉do
    18: if(ts,rank(self)) = b then
    19:     promises := promises U (a, v)
    20:     if #promises = ((N+1)/2) then
    21:         (maxBallot, value) := HighestByBallot(promises)
    22:         pv := value if value != ⊥ else pv
    23:         trigger〈beb,Broadcast|[Accept,(ts,rank(self)), pv]〉
    */
    case PL_Deliver(src, prepAck @ Promise(promiseBallot, acceptedBallot, acceptedValue) ) => handle {
      if ((round, rank) == promiseBallot) {
          promises += ((acceptedBallot, acceptedValue));
          
          val v = promises.size
          println(s"$src -> $self - $prepAck - incremeting promises - $v");
          
          if(promises.size == ((numProcesses+1)/2)){
              var (maxBallot, value) = promises.maxBy(_._1);
              proposedValue = value match{
                  case None => proposedValue
                  case x => x
              }
              
              println(s"$src -> $self - $prepAck - accepting $proposedValue");
              trigger (BEB_Broadcast ( Accept((round, rank), proposedValue.get) ) -> beb)
          }
      } else {
          println(s"$src -> $self - Ignoring $prepAck");
      }
    };

    /*
    24:upon event〈pp2p,Deliver|p,[Accepted, ballot]〉do
    25:     if((ts,rank(self)) = ballot then
    26:         numOfAccepts := numOfAccepts + 1
    27:         if numOfAccepts = ((N+1)/2) then
    28:             trigger <beb,Broadcast|[Decided, pv]>
    */
    case PL_Deliver(src, accAck @ Accepted(acceptBallot) ) => handle {
      if ((round, rank) == acceptBallot) {
        numOfAccepts = numOfAccepts + 1;
        val needed = math ceil ((numProcesses+1)/2.0)
        println(s"$src -> $self - $numOfAccepts/$needed of $numProcesses - increasing num of accepts");
        
        if(numOfAccepts == needed){
            println(s"$src -> $self ------------------------------------------- DECIDED $proposedValue");
            trigger (BEB_Broadcast (Decided(proposedValue.get)) -> beb);
        }
      }
    };

    /*
    42:upon event〈pp2p,Deliver|p,[Nack, ballot]〉do
    43:     if ((ts,Rank(self)) = ballot then
    44:         Propose()
    */
    case PL_Deliver(src, nack @ Nack(ballot)) => handle {
      if ((round, rank) == ballot) {
        propose()
      }
    }
  }
  
 
};