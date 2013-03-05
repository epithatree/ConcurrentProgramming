import ox.CSO._

object Phils{
  val N = 5; // Number of philosophers

  val random = new scala.util.Random;

  // Simulate basic actions
  def Eat = sleep(500);
  def Think = sleep(random.nextInt(800)); 
  def Pause = sleep(500);
 


  // A single philosopher
  def Phil(me : Int, left: ![String], right: ![String]) = proc("Phil"+me){
    repeat{
      Think;
      report!(me+" sits"); Pause;
      right!"pick"; report!(me+" picks up right fork"); Pause;
      left!"pick"; report!(me+" picks up left fork"); Pause;
      report ! (me+" eats"); Eat;
      left!"drop"; Pause;
      right!"drop"; Pause;
      report ! (me+" leaves")
    }
  }

  // A right handed philosopher
  def RPhil(me : Int, left: ![String], right: ![String]) = proc("Phil"+me){
    repeat{
      Think;
      report!(me+" sits"); Pause;
      left!"pick"; report!(me+" picks up left fork"); Pause;
      right!"pick"; report!(me+" picks up right fork"); Pause;
      report ! (me+" eats"); Eat;

      right!"drop"; Pause;
      left!"drop"; Pause;

      report ! (me+" leaves")
    }
  }
  // A single fork
  def Fork(me : Int, left: ?[String], right: ?[String]) = proc("Fork"+me){
    serve(
      left --> {
	val x = left?; assert(x=="pick");
	val y = left?; assert(y=="drop");
      }
      | right --> {
	val x = right?; assert(x=="pick");
	val y = right?; assert(y=="drop");
      }
    )
  }

  // Copy messages from report onto the console
  def TheConsole : PROC = proc{ repeat{ Console.println(report?) } }

  // Channels to pick up and drop the forks:
  val philToLeftFork, philToRightFork = OneOne[String](5) 
  // philToLeftFork(i) is from Phil(i) to Fork(i);
  // philToRightFork(i) is from Phil(i) to Fork((i-1)%N)


  // channel to report what's happening 
  val report = ManyOne[String]; 
  // Put the components together
  def AllPhils : PROC = || ( 
    for (i <- 0 until N-1) yield 
      Phil( i, philToLeftFork(i), philToRightFork(i) ) 
  )
  def AllForks : PROC = || ( 
    for (i <- 0 until N) yield 
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i) ) 
  )

  def System : PROC = AllPhils || AllForks || TheConsole || RPhil(N-1, philToLeftFork(N-1), philToRightFork(N-1))

  // And run it
  def main() = System() 
}

  
