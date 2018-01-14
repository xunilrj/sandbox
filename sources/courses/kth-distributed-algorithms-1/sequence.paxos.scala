
class SequencePaxos(init: Init[SequencePaxos]) extends ComponentDefinition {

import Role._
import State._
    
  val sc = provides[SequenceConsensus];
  val ble = requires[BallotLeaderElection];
  val pl = requires[FIFOPerfectLink];

  val (self, pi, others) = init match {
    case Init(addr: Address, pi: Set[Address] @unchecked) => (addr, pi, pi - addr)
  }
  val majority = (pi.size / 2) + 1;

  var state = (FOLLOWER, UNKOWN);
  var nL = 0l;
  var nProm = 0l;
  var leader: Option[Address] = None;
  var na = 0l;
  var va = List.empty[RSM_Command];
  var ld = 0;
  // leader state
  var propCmds = List.empty[RSM_Command];
  val las = mutable.Map.empty[Address, Int];
  pi foreach (x => {las += (x -> 0)});
  val lds = mutable.Map.empty[Address, Int];
  var lc = 0;
  val acks = mutable.Map.empty[Address, (Long, List[RSM_Command])];


    def maxLcQuorum(vaa : Int): Int =
    {
        var lcc = lc;
        var cont = lcc < vaa;
        while(cont)
        {
            val count = pi count (x => {las(x) >= lcc});
            if(count >= majority)
            {
                lcc = lcc + 1;
                cont = lcc < vaa;
            }
            else {
                lcc = lcc - 1;
                cont = false;
            }
        }
        return lcc;
    }

  ble uponEvent {
        /*
            13:upon event〈ble,Leader|L, n〉do
            14: if n > nL then
            15:     leader:=L
            16:     nL:=n
            17:     if self=L ∧ nL > nprom then
            18:         state:= (leader,prepare)
            19:         propCmds:=〈〉
            20:         las:= [0]N
            21:         lds:= [⊥]N
            22:         acks:= [⊥]N
            23:         lc:= 0
            24:         for allp∈Π\{self}do
            25:             trigger〈fifop2p,Send|p,[Prepare,nL,ld,na]〉
            26:         acks[L]:= (na,suffix(va,ld))
            27:         lds[self]:=ld
            28:         nprom:=nL
            29:     else
            30:         state:= (follower,state.2)
        */
        case msg @ BLE_Leader(l, n) => handle {
            println(s"$self $msg");
            if (n > nL) {
                leader = Some(l);
                nL = n;
                if(self == l && nL > nProm) {
                    state = (LEADER, PREPARE);
                    propCmds = List.empty[RSM_Command];
                    las.clear(); pi foreach (x => {las += (x -> 0)});
                    lds.clear();
                    acks.clear();
                    lc = 0;
                    pi foreach (x => {
                        if(x != self){
                            trigger(PL_Send(x, Prepare(nL, ld, na)) -> pl);
                        }
                    })
                    acks += (l -> (na, suffix(va,ld)));
                    lds += (self -> ld);
                    nProm = nL;
                }
                else {
                    state = (FOLLOWER,state._2)
                }
            }
        }
   }
   
  pl uponEvent {
        /*
        31:upon event〈fifop2p,Deliver|p,[Prepare, np,ldL, n]〉do
        32: if nprom< np then
        33:     nprom:=np
        34:     state:= (follower,prepare)
        35:     if na≥n then
        36:         sfx:= suffix(va,ldL)
        37:     else
        38:         sfx:=〈〉
        39:     trigger〈fifop2p,Send|p,[Promise,np,na,sfx, ld]〉
        */
        case msg @ PL_Deliver(p, Prepare(np, ldL, naL)) => handle {
            println(s"$self $msg");
            if(nProm < np){
                nProm = np;
                state = (FOLLOWER,PREPARE);
                var sfx = List.empty[RSM_Command];
                if(na >= naL){
                    sfx = suffix(va,ldL);
                }
                trigger(PL_Send(p, Promise(np, na, sfx, ld)) -> pl);
            }
        }
        
        /*
        40:upon event〈fifop2p,Deliver|a,[Promise,n,na,sfxa,lda]〉do
        41: if n=nL ∧ state=(leader,prepare) then
        42:     acks[a]:= (na,sfxa)
        43:     lds[a]:=lda
        44:     P:={p∈Π|acks[p]!=⊥}
        45:     if|P|=⌈N+12then
        46:         (k,sfx):= max{acks[p]|p∈P}.adopt v
        47:         va:= prefix(va, ld) +sfx+propCmds
        48:         las[self]:=|va|
        49:         propCmds:=〈〉
        50:         state:= (leader,accept)
        51:         for allp∈{p∈Π|lds[p]!=⊥∧p!=self}do
        52:             sfxp:= suffix(va,lds[p])
        53:             trigger〈fifop2p,Send|p,[AcceptSync,nL,sfxp,lds[p]]〉
        54: else ifn=nL∧state= (leader,accept)then
        55:     lds[a]:=lda
        56:     sfx:= suffix(va,lds[a])
        57:     trigger〈fifop2p,Send|a,[AcceptSync,nL,sfx,lds[a]]〉
        58:     if lc!= 0 then
        59:         trigger〈fifop2p,Send|a,[Decide,ld,nL]〉
        */
    case PL_Deliver(a, Promise(n, naA, sfxA, ldA)) => handle {
      if ((n == nL) && (state == (LEADER, PREPARE))) {
        acks += (a -> (naA, sfxA));
        lds += (a -> ldA);
        if(acks.size == majority){
            val (k,sfx) = acks.values.maxBy(x => {(x._1,x._2.size)});
            va = prefix(va, ld) ++ sfx ++ propCmds;
            las(self) = va.size;
            propCmds = List.empty;
            state = (LEADER, ACCEPT);
            pi foreach (x => {
               if((lds contains x) && (x != self)){
                   val sfxp = suffix(va, lds(x));
                   trigger(PL_Send(x, AcceptSync(nL,sfxp,lds(x))) -> pl);
               } 
            });
        }
      } else if ((n == nL) && (state == (LEADER, ACCEPT))) {
        lds(a) = ldA;
        val sfx = suffix(va, lds(a));
        trigger(PL_Send(a, AcceptSync(nL,sfx,lds(a))) -> pl);
        if(lc != 0){
            if(ld > las(a)) println(s"WARNING $a $ld > ${las(a)}");
            trigger(PL_Send(a, Decide(ld,nL)) -> pl);
        }
      }
    }
    
    /*
    60:upon event〈fifop2p,Deliver|p,[AcceptSync, nL,sfxv,ld]〉do
    61: ifstate= (follower,prepare)then
    62:     ifnprom=nLthen
    63:         na:=nL
    64:         va:= prefix(va,ld) +sfxv
    65:         trigger〈fifop2p,Send|p,[Accepted,nL,|va|]〉
    66:         state:= (follower,accept)
    */
    case msg @ PL_Deliver(p, AcceptSync(nL, sfx, ldp)) => handle {
        println(s"$self $msg ${va.size}");
      if ((nProm == nL) && (state == (FOLLOWER, PREPARE))) {
         na = nL;
         va = prefix(va,ldp) ++ sfx;
         trigger(PL_Send(p, Accepted(nL, va.size)) -> pl);
         state = (FOLLOWER,ACCEPT);
         println(s"$self at $state ${va.size}");
      }
      else{
        println(s"$self IGNORING ACCEPTASYNC $nProm $state $msg");
      }
    }
    /*
    67:upon event〈fifop2p,Deliver|p,[Accept, nL, C]〉do
    68: ifstate= (follower,accept)then
    69:     ifnprom=nLthen
    70:         va:=va+〈C〉
    71:         trigger〈fifop2p,Send|p,[Accepted,nL,|va|]〉
    */
    case PL_Deliver(p, Accept(nL, c)) => handle {
      if ((nProm == nL) && (state == (FOLLOWER, ACCEPT))) {
         va = va ++ List(c);
         trigger(PL_Send(p, Accepted(nL,va.size)) -> pl);
      }
    }
    
    /*
    72:upon event〈fifop2p,Deliver|p,[Decide, l, nL]〉do
    73: if nprom=nL then
    74:     while ld< l do
    75:         trigger〈sc,Decide|va[ld]〉
    76:         ld:=ld+ 1
    */
    case msg @ PL_Deliver(_, Decide(lcL, nL)) => handle {
        println(s"$self $msg");
        if(nProm == nL){
            val vasize = va.size;
            println(s"$ld <  $lcL ($vasize)");
            while(ld < lcL){
                val decidemsg = SC_Decide(va(ld));
                println(s"$self $decidemsg");
                trigger(decidemsg -> sc);
                ld = ld + 1;
            }
        } else {
            println(s"ignored Decide $nProm $nL");
        }
    }
    
    
    /*
    85:upon event〈fifop2p,Deliver|a,[Accepted, n, m]〉do
    86: if state= (leader,accept) ∧ n=nL then
    87:     las[a]:=m
    88:     if lc<m ∧ |{p∈Π|las[p]≥m}|≥⌈N+12⌉ then
    89:         lc:=m
    90:         for allp∈{p∈Π|lds[p] != ⊥}do
    91:             trigger〈fifop2p,Send|p,[Decide, lc, nL]〉
    */
    case msg @ PL_Deliver(a, Accepted(n, vaASize)) => handle {
        println(s"$self $msg");
        if ((n == nL) && (state == (LEADER, ACCEPT))) {
            las(a) = vaASize;
            val count = pi count (x => {las(x) >= vaASize});
            if((lc < vaASize)&&(count >= majority)){
                lc = vaASize;
                pi foreach (x =>{
                   if(lds contains x){
                       if(lc > las(x)) println(s"WARNING $x $lc > ${las(x)}");
                       trigger(PL_Send(x, Decide(lc, nL)) -> pl);
                   } 
                });
            }
        }
    }
  }

  sc uponEvent {
    /*
    77:upon event〈sc,Propose|C〉do
    78: if state= (leader,prepare) then
    79:     propCmds:=propCmds+〈C〉
    80: else ifstate= (leader,accept)then
    81:     va:=va+〈C〉
    82:     las[self]:=las[self] + 1
    83:     for allp∈{p∈Π|lds[p]!=⊥∧p!=self}do
    84:         trigger〈fifop2p,Send|p,[Accept,nL, C]〉
    */
    case SC_Propose(c) => handle {
      if (state == (LEADER, PREPARE)) {
         propCmds = propCmds ++ List(c);
      } 
      else if (state == (LEADER, ACCEPT)) {
         va = va ++ List(c);
         las(self) = las(self) + 1;
         pi foreach (x => {
            if((lds contains x)&&(x != self)){
                trigger(PL_Send(x, Accept(nL,c)) -> pl);
            } 
         });
      }
    }
  }
}