package Mips.Mux4

import NewHDL.Core.HDLBase._


class Mux4 (a0:HDL[Unsigned],a1:HDL[Unsigned],
              a2:HDL[Unsigned],a3:HDL[Unsigned],
               s:HDL[Unsigned],y:HDL[Unsigned]) extends HDLClass {

  def mux4(a0:HDL[Unsigned],a1:HDL[Unsigned],
              a2:HDL[Unsigned],a3:HDL[Unsigned],
              s:HDL[Unsigned],y:HDL[Unsigned]) ={
    async {
      when(s is 0) {
        y := a0
      }.elsewhen(s is 1){
        y := a1
      }.elsewhen(s is 2){
        y := a2
      }.otherwise {
        y := a3
      }
    }
  }

  def mux4_module = module{
    mux4(a0,a1,a2,a3,s,y)
  }

  override val toCompile = List(mux4_module)
}

object Main {
  def main(args: Array[String]): Unit = {
    new Mux4(Unsigned(0, 32), Unsigned(0, 32),
      Unsigned(0, 32), Unsigned(0, 32),
      Unsigned(0, 2), Unsigned(0, 32)).compile.toConsole
  }

}
