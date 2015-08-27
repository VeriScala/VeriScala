package Mips.Cpu

import NewHDL.Core.HDLBase._


class Cpu(clock:HDL[Boolean],resetn:HDL[Boolean],
           inst:HDL[Unsigned],mem:HDL[Unsigned],
           pc:HDL[Unsigned],wmem:HDL[Boolean],
           alu:HDL[Unsigned],data:HDL[Unsigned]) extends HDLClass{

  def cpu(clock:HDL[Boolean],resetn:HDL[Boolean],
          inst:HDL[Unsigned],mem:HDL[Unsigned],
          pc:HDL[Unsigned],wmem:HDL[Boolean],
          alu:HDL[Unsigned],data:HDL[Unsigned]): Unit ={

    val p4 = HDLlize(Unsigned(0,32))
    val bpc = HDLlize(Unsigned(0,32))
    val npc = HDLlize(Unsigned(0,32))
    val adr = HDLlize(Unsigned(0,32))
    val ra = HDLlize(Unsigned(0,32))
    val alua = HDLlize(Unsigned(0,32))
    val alub = HDLlize(Unsigned(0,32))
    val res = HDLlize(Unsigned(0,32))
    val alu_mem = HDLlize(Unsigned(0,32))

    val aluc = HDLlize(Unsigned(0,4))

    val reg_dest = HDLlize(Unsigned(0,5))
    val wn = HDLlize(Unsigned(0,5))

    val pcsource = HDLlize(Unsigned(0,2))

    val zero = HDLlize(b0)
    val wmem = HDLlize(b0)
    val wreg = HDLlize(b0)
    val regrt = HDLlize(b0)
    val m2reg = HDLlize(b0)
    val shift = HDLlize(b0)
    val aluimm = HDLlize(b0)
    val jal = HDLlize(b0)
    val sext = HDLlize(b0)

    val e = HDLlize(b0)
    val imm = HDLlize(Unsigned(0,16))

    val sa = HDLlize(Unsigned(0,32))
    val offset = HDLlize(Unsigned(0,32))
    val immediate = HDLlize(Unsigned(0,32))

    async{
      
    }
  }
}
