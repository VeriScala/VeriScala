package NewHDLExample.Arithmetic.Add

import NewHDL.Core.HDLBase._

class Adder[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T]) extends HDLClass {
  def add = module {
    sync(clk, 1) {
      when (rst) {
        z := 0
      } .otherwise {
        z := a + b
      }
    }
  }

  override val toCompile = List(add)
}

object Main {
  def main(args: Array[String]) {
    val handle_Adder = new Adder(false, false,
      Signed(0, 5), Signed(1, 5), Signed(0, 6))
    println(handle_Adder.compile)

    // 8081 is FPGA(Simulator) port, 8082 is ScalaHDL host port
    //handle_Adder.network_debug_run("59.78.56.59", 8081, 8082)
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
