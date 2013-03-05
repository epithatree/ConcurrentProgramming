import ox.CSO._

object Phils{
  val N = 5; // Number of philosophers

  val random = new scala.util.Random;

  // Simulate basic actions
  def Eat = sleep(500);
  def Think = sleep(random.nextInt(800)); 
  def Pause = sleep(500);
 
  val report = ManyOne[String]; 

  // A single philosopher
  def Phil(me : Int, left: ![String], right: ![String], toButlerRequest: ![Int], toButlerRelease: ![Int], fromButler : ?[String]) = proc("Phil"+me){
    repeat {
      Think;
      toButlerRequest!(me); 
      val x =fromButler?; assert(x=="sitdown");
      report!(me+" sits"); Pause;
      left!"pick"; report!(me+" picks up left fork"); Pause;
      right!"pick"; report!(me+" picks up right fork"); Pause;
      report ! (me+" eats"); Eat;
      left!"drop"; Pause;
      right!"drop"; Pause;
      report ! (me+" leaves");
      toButlerRelease!(me); 
      val y = fromButler?; assert(y=="standup");
    }
  }
  
    // A butler
  def Butler(requests: ?[Int], permissions: Seq[![String]], release: ?[Int]) = proc("Butler"){
    var n = 0
    repeat {
    alt( ((n <4) &&& requests) --> {val c = requests?; permissions(c)!"sitdown"; n=n+1}
      | (true &&& release) --> {val c = release?; permissions(c)!"standup"; n=n-1}
   ) 
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
  
  //Channels for talking to the butler
  val requestSeat = ManyOne[Int]
  val releaseSeat = ManyOne[Int]
  val butlerReply = OneOne[String](5)

  // channel to report what's happening  // Put the components together
  def AllPhils : PROC = || ( 
    for (i <- 0 until N) yield 
      Phil( i, philToLeftFork(i), philToRightFork(i), requestSeat, releaseSeat, butlerReply(i) ) 
  )

  def AllForks : PROC = || ( 
    for (i <- 0 until N) yield 
      Fork( i, philToRightFork((i+1)%N), philToLeftFork(i)) 
  )

  def System : PROC = AllPhils || AllForks || TheConsole || Butler(requestSeat, butlerReply, releaseSeat)

  // And run it
  def main() = System() 
}

  
