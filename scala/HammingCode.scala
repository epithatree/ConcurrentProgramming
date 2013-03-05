

import  ox.CSO._
import  ox.cso.Components

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
  }

  def x3(in: ?[Int], out: ![Int]) : PROC = proc{
      repeat{
      val v = in?; out!(3*v);
    }
  }

  def x5(in: ?[Int], out: ![Int]) : PROC = proc{
    repeat{
      val v = in?; out!(5*v);
    }
  }

  def Merge(in1: ?[Int], in2: ?[Int], out: ![Int]) : PROC = proc{
  repeat {
        (proc{out!(in1?)} || proc{out!(in2?)}) ()
        }
  in1.closein; in2.closein; out.closeout;
  }

  def Prefix1(in: ?[Int], out: ![Int]) : PROC = proc{
  out!1;
  repeat {
        out!(in?)
        }
  in.closein; out.closeout;
  }
  
  // Copy messages from output onto the console
  def TheConsole : PROC = proc{ repeat{ Console.println(output?) } }

  val x2Input, x3Input, x5Input, x2Output, x3Output, x5Output, mergeToMerge, mergeToPrefix, tee1ToTee2, tee2ToTee3, output = OneOne[Int];
  val prefixToTee = Buf[Int](4);

  def System =
    Prefix1(mergeToPrefix, prefixToTee) || Tee(prefixToTee, tee1ToTee2, output) || Tee(tee1ToTee2, tee2ToTee3, x2Input)  || Tee(tee2ToTee3, x3Input, x5Input) || x2(x2Input, x2Output) || x3(x3Input, x3Output) || x5(x5Input, x5Output) || Merge(x2Output, x3Output, mergeToMerge) || Merge(mergeToMerge, x5Output, mergeToPrefix)||TheConsole

  def main() = System()
}

