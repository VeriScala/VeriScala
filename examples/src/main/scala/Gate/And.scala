package NewHDLExample.Gate.And

import NewHDL.Core.HDLBase._

class And[T](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T]) extends HDLClass {
  def and = module {
    sync(clk, 1) {
      when (rst) {
        z := 0
      } .otherwise {
        z := a & b
      }
    }
  }

  override val toCompile = List(and)
}

object Main {
  def main(args: Array[String]) {
    new And(false, false, false, false, false).compile.toConsole
  }
}
