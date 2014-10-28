package NewHDLExample.Arithmetic

import NewHDL.Core.HDLClass
import NewHDL.Core.HDLBase._
import NewHDL.Core.Arith
import NewHDL.Core.ArithCompiler

class Adder[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T]) extends HDLClass
    with Arith with ArithCompiler {
  def add = module {
    async {
      when (rst) {
        z := 0
      } .otherwise {
        z := a + b
      }
    }
  }

  def compile: String = compile(add)
}

object Main {
  def main(args: Array[String]) {
    println((new Adder(0, 0,
      Signed(0, 5), Signed(1, 5), Signed(0, 6))).compile)
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
