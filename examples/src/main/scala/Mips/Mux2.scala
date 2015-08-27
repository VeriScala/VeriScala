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
                s:HDL[Boolean],y:HDL[Unsigned]) extends HDLClass {

  def mux2(a0:HDL[Unsigned],a1:HDL[Unsigned],
              s:HDL[Boolean],y:HDL[Unsigned]) ={
    async {
      when(s is 1) {
        y := a1
      }.otherwise {
        y := a0
      }
    }
  }

  def mux2_module = module{
    mux2(a0,a1,s,y)
  }

  override val toCompile = List(mux2_module)
}

object Main {
  def main(args:Array[String]): Unit ={
    println(new Mux2(Unsigned(0,32),Unsigned(0,32),
    b0,Unsigned(0,32)).compile)
  }
}