package NewHDLExample.Gate.Or

import NewHDL.Core.HDLBase._

class Or[T](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T]) extends HDLClass {
  def or = module {
    sync(clk, 1) {
      when (rst) {
        z := 0
      } .otherwise {
        z := a | b
      }
    }
  }

  override val toCompile = List(or)
}

object Main {
  def main(args: Array[String]) {
    println(new Or[Boolean](0, 0, 0, 0, 0).compile)
  }
}
