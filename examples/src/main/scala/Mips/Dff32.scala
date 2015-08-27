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

import NewHDL.Core.HDLBase.{HDLClass, HDL, Unsigned}


class Dff32 (d:HDL[Unsigned],clk:HDL[Boolean],
              clrn:HDL[Boolean],q:HDL[Unsigned]) extends HDLClass {

  def dff32(d:HDL[Unsigned],clk:HDL[Boolean],
            clrn:HDL[Boolean],q:HDL[Unsigned])={

  }

}
