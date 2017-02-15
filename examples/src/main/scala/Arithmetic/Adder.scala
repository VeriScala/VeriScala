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
      Signed(0, 5), Signed(0, 5), Signed(0, 6))
    println(handle_Adder.compile)

    /*handle_Adder.network_on_off = true
    val conf : Config = ConfigFactory.load()
    handle_Adder.network_debug_run()*/
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
