
class ReadImposeWriteConsultMajority(init: Init[ReadImposeWriteConsultMajority]) extends ComponentDefinition {

  //subscriptions

  val nnar = provides[AtomicRegister];

  val pLink = requires[PerfectLink];
  val beb = requires[BestEffortBroadcast];

  //state and initialization
  println(s"INIT");
  val (self: Address, n: Int, selfRank: Int) = init match {
    case Init(selfAddr: Address, n: Int) => (selfAddr, n, AddressUtils.toRank(selfAddr))
  };

  var (ts, wr) = (0, 0);
  var value: Option[Any] = None;
  var acks = 0;
  var readval: Option[Any] = None;
  var writeval: Option[Any] = None;
  var rid = 0;
  var readlist: Map[Address, (Int, Int, Option[Any])] = Map.empty
  var reading = false;

  //handlers

  nnar uponEvent {
    /*
    09: upon event <nnar,Read> do
    10:     rid := rid + 1
    11:     acks := 0
    12:     ∀p ∈ Π readlist[p] := ⊥ 
    13:     reading := True
    14:     trigger <beb,Broadcast | [Read,rid]>
    */
    case msg @ AR_Read_Request() => handle {
        println(s"$self $msg");
        rid = rid + 1;
        acks = 0;
        readlist = Map.empty;
        reading = true;
        trigger(BEB_Broadcast(READ(rid)) -> beb);
    };
    /*
    30: upon event h nnar,Write | v i do 
    31:     rid := rid + 1 
    32:     writeval := v 
    33:     acks := 0 
    34:     ∀p∈Π readlist[p] := ⊥ 
    35:     trigger h beb,Broadcast | [Read,rid] i
    */
    case msg @ AR_Write_Request(wval) => handle {
        println(s"$self $msg");
        rid = rid + 1;
        writeval = Some(wval);
        acks = 0;
        readlist = Map.empty;
        trigger(BEB_Broadcast(READ(rid)) -> beb);
    }
  }

  beb uponEvent {
    /*
    15: upon event h beb,Deliver | p,[Read,r] i do
    16:     trigger h pp2p,Send | p,[Value,r,ts,wr,val] i 
    */
    case msg @ BEB_Deliver(src, READ(r)) => handle {
        println(s"$self $msg");
        trigger(PL_Send(src, VALUE(r, ts, wr, value)) -> pLink);
    }
    /*
    36: upon event h beb,Deliver | p,[Write,r,ts0,wr0,v0] i do
    37: if (ts0,wr0) > (ts,wr) then . Tuple comparison.
    38:     (ts,wr,val) := (ts0,wr0,v0)
    39: trigger h pp2p,Send | p,[Ack,r] i 
    */
    case msg @ BEB_Deliver(src, w @ WRITE(r,ts0,wr0,v0)) => handle {
        if( (ts0,wr0) > (ts,wr) ){
            ts = ts0;
            wr = wr0;
            value = v0;
            println(s"$self Accepted $msg");
        } else {
            println(s"$self Discarded $msg");
        }
        println(s"$self Current State rid=$rid ts=$ts wr=$wr value=$value");
        trigger(PL_Send(src, ACK(r)) -> pLink);
    }
  }

  pLink uponEvent {
    /*
    17: upon event h pp2p,Deliver | p,[Value,r,ts0,wr0,v0] i do
    18: if r = rid then
    19:     readlist[p] := (ts0,wr0,v0)
    20:     if |readlist| > N/2 then //Where N = |Π|.
    21:         (maxts,rr,readval) := highest(readlist)
    22:         ∀q∈Π readlist[q] := ⊥
    23:         if reading then
    24:             bcastval := readval
    25:         else
    26:             rr := rank(self )
    27:             maxts := maxts + 1
    28:             bcastval := writeval
    29:         trigger h beb,Broadcast | [Write,rid,maxts,rr,bcastval] i
    */
    case msg @ PL_Deliver(src, v @ VALUE(r,ts0,wr0,v0)) => handle {
        println(s"$self $msg");
      var bcastval: Option[Any] = None;
      if (r == rid) {
        readlist += (src -> (ts0,wr0,v0));
        if(readlist.size > n/2){
            var (_,(maxts, rr, rv)) = readlist.maxBy(_._2._1);
            readval = rv;
            readlist = Map.empty;
            if(reading){
                bcastval = rv;
            }else{
                rr = selfRank;
                maxts = maxts + 1;
                bcastval = writeval;
            }
            val msg = BEB_Broadcast(WRITE(rid, maxts,rr,bcastval));
            println(s"$self Aggregating values $msg");
            trigger(msg -> beb);
        }
      }
    }
    /*
    40: upon event hpp2p, Deliver | p, [Ack, r] ido
    41:     if r = rid then
    42:         acks := acks + 1
    43:         if acks > N/2 then
    44:             acks := 0
    45:             if reading then
    46:                 reading := False
    47:                 trigger hnnar , ReadReturn | readval i
    48:             else
    49:                 trigger hnnar , WriteReturn | i
    */
    case msg @ PL_Deliver(src, v @ ACK(r)) => handle {
        println(s"$self $msg acks=$acks rid=$rid");
        if (r == rid) {
            acks += 1;
            if(acks > n/2){
                acks = 0;
                if(reading){
                    reading = false;
                    val responsemsg = AR_Read_Response(readval);
                    println(s"$self $responsemsg");
                    trigger(responsemsg -> nnar);
                } else {
                    val responsemsg = AR_Write_Response();
                    println(s"$self $responsemsg $value");
                    trigger(responsemsg -> nnar);
                }
            }
        }
    }
  }
}