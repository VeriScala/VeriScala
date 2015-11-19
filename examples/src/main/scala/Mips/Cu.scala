package Mips.Cu

import NewHDL.Core.HDLBase._


class Cu(op:HDL[Unsigned],func:HDL[Unsigned],z:HDL[Boolean],
          wmem:HDL[Boolean],
          wreg:HDL[Boolean],
          regrt:HDL[Boolean],
          m2reg:HDL[Boolean],
          aluc:HDL[Unsigned],
          shift:HDL[Boolean],
          aluimm:HDL[Boolean],
          pcsource:HDL[Unsigned],
          jal:HDL[Boolean],
          sext:HDL[Boolean]) extends HDLClass{

  def cu(op:HDL[Unsigned],func:HDL[Unsigned],z:HDL[Boolean],
         wmem:HDL[Boolean],
         wreg:HDL[Boolean],
         regrt:HDL[Boolean],
         m2reg:HDL[Boolean],
         aluc:HDL[Unsigned],
         shift:HDL[Boolean],
         aluimm:HDL[Boolean],
         pcsource:HDL[Unsigned],
         jal:HDL[Boolean],
         sext:HDL[Boolean]) ={

    val i_add = HDLlize(b0)
    val i_sub = HDLlize(b0)
    val i_and = HDLlize(b0)
    val i_or = HDLlize(b0)
    val i_mul = HDLlize(b0)
    val i_xor = HDLlize(b0)
    val i_sll = HDLlize(b0)
    val i_srl = HDLlize(b0)
    val i_sra = HDLlize(b0)
    val i_jr = HDLlize(b0)

    val i_addi = HDLlize(b0)
    val i_andi = HDLlize(b0)
    val i_ori = HDLlize(b0)
    val i_xori = HDLlize(b0)
    val i_lw = HDLlize(b0)
    val i_sw = HDLlize(b0)
    val i_beq = HDLlize(b0)
    val i_bne = HDLlize(b0)
    val i_lui = HDLlize(b0)
    val i_j = HDLlize(b0)
    val i_jal = HDLlize(b0)

    val r_type = HDLlize(b0)


    async{
      r_type := logic2(b1,op,map("allones"))//logic(op,map("allones")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//~(op(0)|op(1)|op(2)|op(3)|op(4)|op(5))
      i_add := logic2(r_type,func,map("i_add"))//logic(func,map("i_add")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&func(5)&(~func(4))&(~func(3))&(~func(2))&(~func(1))&(~func(0))
      i_sub := logic2(r_type,func,map("i_sub"))//logic(func,map("i_sub")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&func(5)&(~func(4))&(~func(3))&(~func(2))&(func(1))&(~func(0))
      i_or := logic2(r_type,func,map("i_or"))//logic(func,map("i_or")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&func(5)&(~func(4))&(~func(3))&(func(2))&(~func(1))&(~func(0))
      i_mul := logic2(r_type,func,map("i_mul"))//logic(func,map("i_mul")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&func(5)&(~func(4))&(~func(3))&(func(2))&(func(1))&(func(0))
      i_xor := logic2(r_type,func,map("i_xor"))//logic(func,map("i_xor")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&func(5)&(~func(4))&(~func(3))&(func(2))&(func(1))&(~func(0))
      i_sll := logic2(r_type,func,map("i_sll"))//logic(func,map("i_sll")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&(~func(5))&(~func(4))&(~func(3))&(~func(2))&(~func(1))&(~func(0))
      i_srl := logic2(r_type,func,map("i_srl"))//logic(func,map("i_srl")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&(~func(5))&(~func(4))&(~func(3))&(~func(2))&(func(1))&(~func(0))
      i_sra := logic2(r_type,func,map("i_sra"))//logic(func,map("i_sra")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&(~func(5))&(~func(4))&(~func(3))&(~func(2))&(func(1))&(func(0))
      i_jr := logic2(r_type,func,map("i_jr"))//logic(func,map("i_jr")).foldLeft(HDLBitwiseAnd(u(1),r_type))((a,b) => a & b)//r_type&(~func(5))&(~func(4))&(func(3))&(~func(2))&(~func(1))&(~ func(0))

      i_addi := logic2(b1,op,map("i_addi"))//logic(op,map("i_addi")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(op(3))&(~op(2))&(~op(1))&(~op(0))
      i_andi :=  logic2(b1,op,map("i_andi"))//logic(op,map("i_andi")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(op(3))&(op(2))&(~op(1))&(~op(0))
      i_ori :=  logic2(b1,op,map("i_ori"))//logic(op,map("i_ori")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(op(3))&(op(2))&(~op(1))&(op(0))
      i_xori :=  logic2(b1,op,map("i_xori"))//logic(op,map("i_xori")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(op(3))&(op(2))&(op(1))&(~op(0))
      i_lw :=  logic2(b1,op,map("i_lw"))//logic(op,map("i_lw")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(op(5))&(~op(4))&(~op(3))&(~op(2))&(op(1))&(op(0))
      i_sw :=  logic2(b1,op,map("i_sw"))//logic(op,map("i_sw")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(op(5))&(~op(4))&(op(3))&(~op(2))&(op(1))&(op(0))
      i_beq :=  logic2(b1,op,map("i_beq"))//logic(op,map("i_beq")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(~op(3))&(op(2))&(~op(1))&(~op(0))
      i_bne :=  logic2(b1,op,map("i_bne"))//logic(op,map("i_bne")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(~op(3))&(op(2))&(~op(1))&(op(0))
      i_lui :=  logic2(b1,op,map("i_lui"))//logic(op,map("i_lui")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(op(3))&(op(2))&(op(1))&(op(0))
      i_j :=  logic2(b1,op,map("i_j"))//logic(op,map("i_j")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(~op(3))&(~op(2))&(op(1))&(~op(0))
      i_jal :=  logic2(b1,op,map("i_jal"))//logic(op,map("i_jal")).foldLeft(HDLBitwiseAnd(u(1),u(1)))((a,b) => a & b)//(~op(5))&(~op(4))&(~op(3))&(~op(2))&(op(1))&(op(0))

      pcsource(1) := i_jr | i_j | i_jal
      pcsource(0) := (i_beq & z) | (i_bne & (~z)) | i_j | i_jal


      wreg := i_add | i_sub | i_and | i_or   | i_xor  |
        i_sll | i_srl | i_sra | i_addi | i_andi |
        i_ori | i_xori | i_lw | i_lui  | i_jal  |
        i_mul



      aluc(3) := i_sra
      aluc(2) := i_sub | i_or | i_ori | i_srl | i_sra | i_bne | i_beq | i_lui
      aluc(1) := i_xor | i_sll | i_srl | i_sra | i_lui | i_xori | i_mul
      aluc(0) := i_and | i_or | i_sll | i_srl | i_sra | i_ori | i_andi | i_mul
      shift   := i_sll | i_srl | i_sra

      aluimm  := i_addi | i_andi | i_ori | i_xori | i_lw | i_sw | i_lui
      sext    := i_addi | i_beq | i_bne | i_lw | i_sw
      wmem    := i_sw
      m2reg   := i_lw
      regrt   := i_addi | i_andi | i_ori | i_xori | i_lw | i_sw | i_lui
      jal     := i_jal
    }
  }

  def logic(a:HDL[Unsigned],s:String) ={
    val l = a.length
    (0 until l).zip(s).map(p => if (p._2 == '1') a(p._1) else (~a(p._1)))
  }
  def logic2(r:HDL[Boolean],a:HDL[Unsigned],s:String)={
    logic(a, s).foldLeft(HDLBitwiseAnd(r, u(1)))((a, b) => a & b)
  }
  def cu_module = module {
    cu(op,func,z,wmem,wreg,regrt,m2reg,aluc,shift,aluimm,pcsource,jal,sext)
  }
  val map  = Map(
    "i_add"->"100000",
    "i_sub"->"100010",
    "i_and"->"100100",
    "i_or"->"100101",
    "i_mul"->"100111",
    "i_xor"->"100110",
    "i_sll"->"000000",
    "i_srl"->"000010",
    "i_sra"->"000011",
    "i_jr"->"001000",

    "i_addi"->"001000",
    "i_andi"->"001100",
    "i_ori"->"001101",
    "i_xori"->"001110",
    "i_lw"->"100011",
    "i_sw"->"101011",
    "i_beq"->"000100",
    "i_bne"->"000101",
    "i_lui"->"001111",
    "i_j"->"000010",
    "i_jal"->"000011",

    "allones"->"111111",
    "allzeros"->"000000"
  )
  override val toCompile = List(cu_module)
}

object Main{
  def main(args:Array[String]): Unit ={
    new Cu(Unsigned(0,6),Unsigned(0,6),b0,
    b0, b0, b0, b0,Unsigned(0,4), b0,
    b1, Unsigned(0,2), b0, b0).compile.toConsole
  }

}