package NewHDLExample.Comp

import NewHDL.Core.HDLBase._

class Comparator(clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[Signed], b: HDL[Signed], z: HDL[Signed]) extends HDLClass {
  def comp = module {
    sync(clk, 1) {
      when (rst) {
        z := 0
      } .otherwise {
        when (a > b) {
          z := 1
        } .elsewhen (a < b) {
          z := -1
        } .otherwise {
          z := 0
        }
      }
    }
  }

  override val toCompile = List(comp)
}

object Main {
  def main(args: Array[String]) {
    new Comparator(b0, b0,
      Signed(0, 5), Signed(0, 5), Signed(0, 2)).compile.toConsole
  }
}
