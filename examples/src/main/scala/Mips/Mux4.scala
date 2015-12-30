package Mips.Mux4

import NewHDL.Core.HDLBase._
import Mips.Mux2.Mux2

class Mux4 (a0:HDL[Unsigned],a1:HDL[Unsigned],
  a2:HDL[Unsigned],a3:HDL[Unsigned],
  s:HDL[Unsigned],y:HDL[Unsigned]) extends Mux2(a0, a1, s, y) {

  def mux4(a0:HDL[Unsigned],a1:HDL[Unsigned],
    a2:HDL[Unsigned],a3:HDL[Unsigned],
    s:HDL[Unsigned],y:HDL[Unsigned]) = {
    mux(List(a0, a1, a2, a3), List(0, 1, 2), s, y)
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
