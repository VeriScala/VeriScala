/*
module dff32 (d,clk,clrn,q);
   input  [31:0] d;
   input         clk,clrn;
   output [31:0] q;
   reg [31:0]    q;
   always @ (negedge clrn or posedge clk)
      if (clrn == 0) begin

          // q <=0;
          q <= -4;
      end else begin
          q <= d;
      end
endmodule
 */
package Mips

import NewHDL.Core.HDLBase._

class Dff32 (d:HDL[Unsigned],clk:HDL[Boolean],
              clrn:HDL[Boolean],q:HDL[Unsigned]) extends HDLClass {

  def dff32(d:HDL[Unsigned],clk:HDL[Boolean],
            clrn:HDL[Boolean],q:HDL[Unsigned]):HDLBlock={
    sync((clrn,0),(clk,1)){
      when (clrn is 0) {
        q := -4
      }.otherwise{
        q := d
      }
    }
  }

  def dff32_module = module {
    dff32(d,clk,clrn,q)
  }
  override val toCompile = List(dff32_module)
}

object Main{
  def main(args: Array[String]): Unit ={
    println(new Dff32(Unsigned(0,32),b0,b0,Unsigned(0,32)).compile)
  }
}