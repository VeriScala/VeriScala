/*
module mux2x32 (a0,a1,s,y);

   input [31:0] a0,a1;
   input        s;

   output [31:0] y;

   assign y = s ? a1 : a0;

endmodule
 */

package Mips.Mux2

import NewHDL.Core.HDLBase._


class Mux2 (a0:HDL[Unsigned],a1:HDL[Unsigned],
  s:HDL[Unsigned],y:HDL[Unsigned]) extends HDLClass {

  def mux(a: List[HDL[Unsigned]], l: List[Int],
    s: HDL[Unsigned], y: HDL[Unsigned]) = {
    async {
      a.zip(l).tail.foldLeft(when (s is l(0)) {
        y := a(0)
      })((st, al) => st.elsewhen(s is al._2) {
        y := al._1
      }).otherwise {
        y := a.last
      }
    }
  }

  def mux2(a0:HDL[Unsigned],a1:HDL[Unsigned],
    s:HDL[Unsigned],y:HDL[Unsigned]) = {
    mux(List(a0, a1), List(1), s, y)
  }

  def mux2_module = module{
    mux2(a0,a1,s,y)
  }

  override val toCompile = List(mux2_module)
}

object Main {
  def main(args:Array[String]): Unit ={
    new Mux2(Unsigned(0,32),Unsigned(0,32),
      u(0),Unsigned(0,32)).compile.toConsole
  }
}
