import  ox.CSO._
import  ox.cso.Components
import scala.collection.immutable

object HammingCode{

  def Tee(in: ?[Int], out1: ![Int], out2: ![Int]) : PROC = proc{
        serve (out1 -!-> { val v = in?; out1!v ;out2! v }
    | out2 -!-> { val v = in?; out2!v; out1!v })
  in.closein; out1.closeout; out2.closeout;
  }

   def x2(in: ?[Int], out: ![Int]) : PROC = proc{
    repeat{
      val v = in?; out!(2*v);
    }
  in.closein; out.closeout;
  }

  def x3(in: ?[Int], out: ![Int]) : PROC = proc{
      repeat{
      val v = in?; out!(3*v);
    }
  in.closein; out.closeout;
  }

  def x5(in: ?[Int], out: ![Int]) : PROC = proc{
    repeat{
      val v = in?; out!(5*v); 
    }
  in.closein; out.closeout;
  }

  def Merge(in1: ?[Int], in2: ?[Int], out: ![Int]) : PROC = proc{
  var table = new immutable.TreeSet[Int]() 
  table = table+0
  repeat {
    var first, second = 0;
    alt(in1 --> {var first = in1?;} 
      | in2 --> {var first = in2?;}) 
    alt(in1 --> {var second = in1?;} 
      | in2 --> {var second = in2?;}) 
                var temp = 0 ;
                if (first > second) {temp = first; first = second; second = temp;}
                if (!table.contains(first)){out!(first); table=(table+first)}
                if (!table.contains(second)){out!(second); table=(table+second)}
        }
  in1.closein; in2.closein; out.closeout;
  }

  def Prefix1(in: ?[Int], out: ![Int]) : PROC = proc{
  var n = 0; 
  out!1; n+=1;
  for (n <- 1 to 1000){
        out!(in?); 
        }
  Console.println("done")
  in.closein; out.closeout;
  }
  
  // Copy messages from output onto the console
  def TheConsole : PROC = proc{ repeat{ Console.println(output?) } }

  val x2Input, x3Input, x5Input, x3Output, mergeToMerge, mergeToPrefix, tee1ToTee2, tee2ToTee3, output = OneOne[Int];
  val prefixToTee = Buf[Int](1000);
  val x2Output, x5Output = Buf[Int](1);

  def System =
    Prefix1(mergeToPrefix, prefixToTee) || Tee(prefixToTee, tee1ToTee2, output) || Tee(tee1ToTee2, tee2ToTee3, x2Input)  || Tee(tee2ToTee3, x3Input, x5Input) || x2(x2Input, x2Output) || x3(x3Input, x3Output) || x5(x5Input, x5Output) || Merge(x2Output, x3Output, mergeToMerge) || Merge(mergeToMerge, x5Output, mergeToPrefix)||TheConsole

  def main() = System()
}

