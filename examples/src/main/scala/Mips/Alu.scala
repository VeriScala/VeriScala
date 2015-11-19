/*
module alu (a,b,aluc,s,z);
   input [31:0] a,b;
   input [3:0] aluc;
   output [31:0] s;
   output        z;
   reg [31:0] s;
   reg        z;
   always @ (a or b or aluc)
      begin                                   // event
         casex (aluc)
             4'bx000: s = a + b;              //x000 ADD
             4'bx100: s = a - b;              //x100 SUB
             4'bx001: s = a & b;              //x001 AND
             4'bx101: s = a | b;              //x101 OR
             4'bx010: s = a ^ b;              //x010 XOR
			 4'bx011: s = a * b;					 //x011 MUL
             4'bx110: s = b << 16;            //x110 LUI: imm << 16bit
             4'b0011: s = b << a;             //0011 SLL: rd <- (rt << sa)
             4'b0111: s = $unsigned(b) >>> a;             //0111 SRL: rd <- (rt >> sa) (logical)
             4'b1111: s = $signed(b) >>> a;   //1111 SRA: rd <- (rt >> sa) (arithmetic)
             default: s = 0;
         endcase
         if (s == 0 )  z = 1;
            else z = 0;
      end
endmodule
 */


package Mips.Alu

import NewHDL.Core.HDLBase._

class Alu (a: HDL[Unsigned], b:HDL[Unsigned], aluc:HDL[Unsigned],
            s:HDL[Unsigned], z:HDL[Boolean]) extends HDLClass{

  def alu(a: HDL[Unsigned], b:HDL[Unsigned], aluc:HDL[Unsigned],
          s:HDL[Unsigned], z:HDL[Boolean]) ={
    async {
      when(aluc is 0){
        s := a + b;
      }.elsewhen((aluc is 4) | (aluc is 12)) {
        s := a - b
      }.elsewhen((aluc is 1) | (aluc is 9)){
        s := a & b
      }.elsewhen((aluc is 5) | (aluc is 13)){
        s := a | b
      }.elsewhen((aluc is 2) | (aluc is 10)){
        s := a ^ b
      }.elsewhen((aluc is 3) | (aluc is 11)){
        s := a * b
      }/*.elsewhen(aluc == 6 | aluc == 14){
        s := b << 16
      }.elsewhen(aluc == 3){
        s := b << a
      }.elsewhen(aluc == 7){
        s := b >> a
      }.elsewhen(aluc == 15){
        s := b >> a
      }*/.otherwise{
        s := 0
      }
    }
    async{
      when(s is 0){
        z := 1
      }.otherwise{
        z := 0
      }
    }

  }
  def alu_module = module {
    alu(a,b,aluc,s,z)
  }
  override val toCompile = List(alu_module)
}

object Main{
  def main(args: Array[String]): Unit ={
    new Alu(Unsigned(0,32),Unsigned(0,32),Unsigned(0,4),
    Unsigned(0,32),b0).compile.toConsole
  }
}