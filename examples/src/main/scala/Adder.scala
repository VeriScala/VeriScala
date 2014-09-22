package NewHDLExample.Arithmetic.Add

import NewHDL.Core.HDLBase._
import NewHDL.Core.BasicOps
import NewHDL.Core.BasicOpsCompiler

trait Adder { this: BasicOps with BasicOpsCompiler =>
  def add(clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[Boolean], b: HDL[Boolean], z: HDL[Boolean]) = module {
    async {
      when (rst) {
        z := 0
      } .otherwise {
        z := a + b
      }
    }
  }
}

object Main extends Adder with BasicOps with BasicOpsCompiler {
  def main(args: Array[String]) {
    println(compile(add(0, 0, 0, 1, 0)))
  }
}

/*
 module adder (
 clk,
 rst,
 a,
 b,
 z
 );

 input clk;
 input rst;
 input signed [4:0] a;
 input signed [4:0] b;
 output signed [5:0] z;
 reg signed [5:0] z;

 initial begin
 z = 0;
 end

 always @(rst, a, b) begin: _add
 if (rst == 1) begin
 z <= 0;
 end
 else begin
 z <= (a + b);
 end
 end


 endmodule
 */
