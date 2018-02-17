import se.kth.edx.id2203.tutorial.gameoflife._
import se.kth.edx.id2203.validation._
import se.kth.edx.id2203.core._
import java.util.Random;
import se.sics.kompics.sl._
import se.sics.kompics.{ Kompics }

case class CellCInit(x: Int, y: Int, init: State.Initializer) extends se.sics.kompics.Init[Cell]

class CellC(init: CellCInit) extends Cell { // class Cell is a KompicsComponent
    // this cell's location in the grid
    val xPos = init.x;
    val yPos = init.y;
    // get the initial state from the State.Initializer (see below this class)
    private var state: State = init.init.apply(xPos, yPos);
    // initialize our view of our neighbours
    private val view = scala.collection.mutable.Map.empty[Tuple2[Int, Int], State];
    // keep track of the generation we are currently communicating on to not mix up delayed messages
    private var lastGen = -1l;

    // declare our ports (the same one but in both directions for broadcasting to neighbours and receiving their broadcasts)
    val envIn = requires(EnvironmentPort);
    val envOut = provides(EnvironmentPort);
    // handle messages on incoming environment port
    envIn uponEvent {
        case Progress(generation) => handle {
            //println(s"Cell($xPos, $yPos) starting generation $generation as $state"); // uncomment if you want to see some printouts
            // for every generaton broadcast our current state
            trigger(BroadcastState(generation, xPos, yPos, state) -> envOut);
            // and prepare to receiver other's states
            if (lastGen < generation) {
                view.clear();
                lastGen = generation;
            }
        }
        case BroadcastState(generation, x, y, otherState) => handle {
            // same as above, just in case we get another component's state broadcast before the instruction to move to the next generation
            if (lastGen < generation) {
                view.clear();
                lastGen = generation;
            }
            // add the received state to our view
            view += ((x -> y) -> otherState);
            //println(s"Cell($xPos, $yPos) got state $otherState from Cell($x, $y) leading to: $view");  // uncomment if you want to see some printouts
            if (view.size == 8) { // once we get the last state broadcast in a generation (from everyone around us: 3x3-1)
                // count live cells in our neighbourhood
                val liveC = view.values.count {
                    case Alive => true
                    case _     => false
                };
                // apply game of life rules to decide our next state based on current state and live count
                // **********************************
                // ******* STUDENT CODE HERE ********
                // **********************************
                //1. Any live cell with fewer than two live neighbours dies, as if caused by under-population.
                //2. Any live cell with two or three live neighbours lives on to the next generation.
                //3. Any live cell with more than three live neighbours dies, as if by over-population.
                //4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
                state = state match {
                    case Alive => 
                        if(liveC < 2) Dead
                        else if (liveC > 3) Dead
                        else Alive
                    case Dead => 
                        if(liveC == 3) Alive
                        else Dead
                }
            }
        }
    }
}


val rand = new Random(System.currentTimeMillis());
// randomly generate the initial state (easy but boring...try to do something more interesting if you like)
val defaultInit: State.Initializer = (x: Int, y: Int) => {
    if (rand.nextBoolean()) {
        Alive
    } else {
        Dead
    }
}
// just a way to generate a child init object from a method
val toCellCInit: Cell.Initializer = {
    case (x: Int, y: Int, init: State.Initializer) => CellCInit(x, y, init).asInstanceOf[se.sics.kompics.Init[Cell]]
};

val cellclass = classOf[CellC].asInstanceOf[Class[Cell]] // nvm some ugly type magic...Java Kompics is sometimes a bit overspecific on what types it wants
runKompics[ParentC](GameOfLifeInit(defaultInit, cellclass, toCellCInit, numGenerations = 10)); // create the Kompics environment and wait for it to finish

checkExample[ParentC] // generate grading token