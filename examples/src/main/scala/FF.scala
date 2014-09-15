package NewHDLExamples.FlipFlop

import NewHDL.Core.HDLBase._
import NewHDL.Core.BasicOps
import NewHDL.Core.Compiler

trait FF { this: BasicOps with Compiler =>
  def ff(clk: HDL[Boolean], rst: HDL[Boolean],
    q: HDL[Boolean], d: HDL[Boolean]) = module {
    sync(clk, 1) {
      when (rst) {
        q := 0
      } .otherwise {
        q := d
      }
    }
  }
}

object Main extends FF with BasicOps with Compiler {
  def main(args: Array[String]) {
    println(compile(ff(0, 0, 0, 0)))
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
