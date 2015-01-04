package NewHDLExamples.FlipFlop

import NewHDL.Core.HDLBase._

class FF(clk: HDL[Boolean], rst: HDL[Boolean],
  q: HDL[Boolean], d: HDL[Boolean]) extends HDLClass {
  def ff = module {
    sync(clk, 1) {
      when (rst) {
        q := 0
      } .otherwise {
        q := d
      }
    }
  }

  override val toCompile = List(ff)
}

object Main {
  def main(args: Array[String]) {
    println(new FF(false, false, false, false).compile)
  }
}

/*
 expected:
 module FlipFlop (
 clk,
 rst,
 d,
 q,
 );

 input clk;
 input d;
 output q;
 reg q;

 initial begin
   q = 0;
 end

 always @(posedge clk) begin: _logic
   if (rst == 1) begin
     q <= 0;
   end
   else begin
     q <= d;
   end
 end


 endmodule
 */
