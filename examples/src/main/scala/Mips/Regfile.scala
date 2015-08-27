package Mips.Regfile

import NewHDL.Core.HDLBase._


class Regfile (rna:HDL[Unsigned],rnb:HDL[Unsigned],
                d:HDL[Unsigned],wn:HDL[Unsigned],we:HDL[Boolean],
                clk:HDL[Boolean],clrn:HDL[Boolean],
                qa:HDL[Unsigned],qb:HDL[Unsigned]) extends HDLClass{

  def regfile (rna:HDL[Unsigned],rnb:HDL[Unsigned],
               d:HDL[Unsigned],wn:HDL[Unsigned],we:HDL[Boolean],
               clk:HDL[Boolean],clrn:HDL[Boolean],
               qa:HDL[Unsigned],qb:HDL[Unsigned]): Unit ={

  }

}
