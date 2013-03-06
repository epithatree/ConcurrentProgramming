import ox.CSO._

object Game{

  type Task = (Int, Int);
  val N = 100;
  val NUMPROCS = 8;
  
  def Neighbors(x : Int, y: Int):Int={
    var n = 0; var i = 0; var j = 0;
    for (i <- (x-1) to (x+1)){
      for (j <- (y-1) to (y+1)){
        if ((i!=x)||(j!=y)){
        if (a((i+N)%N)((j+N)%N)){n+=1}
      }
      }
    }
  n;
  }

  val a = Array.ofDim[Boolean](N,N)
  val b = Array.ofDim[Boolean](N,N)
  a(1)(1) = true
  a(2)(1) = true
  a(2)(3) = true
  a(4)(2) = true
  a(5)(1) = true 
  a(6)(1) = true
  a(7)(1) = true
  val displayArray = a
  val display = new Display(N,displayArray)
  val barrier = new Barrier(NUMPROCS+1)

  def Worker(me : Int, receiveWork: ?[Task]) = proc("Worker"+me){
    val workPair = receiveWork?
    val x = workPair._1
    val gap = workPair._2 
    repeat {
      for (i <- x until (x+gap)){
        for (j <- 0 until N){
         if (Neighbors(i,j)>3){b(i)(j) = false}
         if (Neighbors(i,j)<2){b(i)(j) = false}
         if (Neighbors(i,j)==3){b(i)(j) = true}
         if (Neighbors(i,j)==2){b(i)(j) = a(i)(j) }
        }
      }
      barrier.sync();
      barrier.sync();
    }
  }
  def Controller(toWorkers: Seq[![Task]]) = proc("Controller"){
    val gap = N/NUMPROCS
    for (i <- 0 until NUMPROCS-1) {
      toWorkers(i)!((i*gap),gap)
    }
    toWorkers(NUMPROCS-1)!(((NUMPROCS-1)*gap),(N+gap-(NUMPROCS*gap)))
   repeat {
     barrier.sync(); 
     for (i <- 0 until N){
       for (j <- 0 until N){
         a(i)(j) = b(i)(j)
       }
     } 
     display.draw
    barrier.sync()
    }
  }
  //Channels
  val controllerToWorkers = OneOne[Task](NUMPROCS)

  def AllWorkers : PROC = || (
    for (i <- 0 until NUMPROCS) yield
      Worker(i, controllerToWorkers(i) ) 
    )
  def System : PROC = AllWorkers || Controller(controllerToWorkers)

  // And run it
  def main() = System()
}


