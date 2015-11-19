package Mips.Regfile

import NewHDL.Core.HDLBase._


class Regfile (rna:HDL[Unsigned],rnb:HDL[Unsigned],
                d:HDL[Unsigned],wn:HDL[Unsigned],we:HDL[Boolean],
                clk:HDL[Boolean],clrn:HDL[Boolean],
                qa:HDL[Unsigned],qb:HDL[Unsigned],depth:Int,width:Int) extends HDLClass{

  def regfile (rna:HDL[Unsigned],rnb:HDL[Unsigned],
               d:HDL[Unsigned],wn:HDL[Unsigned],we:HDL[Boolean],
               clk:HDL[Boolean],clrn:HDL[Boolean],
               qa:HDL[Unsigned],qb:HDL[Unsigned],depth:Int,width:Int) :HDLBlock={

    val registers = HDLlize((1 to depth).map(Unsigned(_, width)))

    async{

    }

    sync((clk,1),(clrn,0)){
      when (rna is 0) {
        qa := 0
      }.otherwise {
        qa := registers(rna - u(1))
      }
      when (rnb is 0) {
        qb := 0
      }.otherwise {
        qb := registers(rnb - u(1))
      }
      when (clrn is 0) {
        for (i <- 0 until depth)
          registers(i) := 0
      }.otherwise{
        when ((wn isnot 0) & (we is 1)){
          registers(wn-u(1)) := d
        }
      }
    }
  }

  def regfile_module = module {
    regfile(rna,rnb,d,wn,we,clk,clrn,qa,qb,depth,width)
  }
  override val toCompile = List(regfile_module)
}

object Main{
  def main (args: Array[String]): Unit ={
    new Regfile(Unsigned(0,5),Unsigned(0,5),Unsigned(0,32),
    Unsigned(0,5),b0,b0,b0,Unsigned(0,32),Unsigned(0,32),31,32).compile.toConsole
  }
}
