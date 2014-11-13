package NewHDLExample.Gate.Xor

import NewHDL.Core.HDLBase._

class Xor[T](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T]) extends HDLClass {
  def xor = module {
    sync(clk, 1) {
      when (rst) {
        z := 0
      } .otherwise {
        z := a ^ b
      }
    }
  }

  override val toCompile = List(xor)
}

object Main {
  def main(args: Array[String]) {
    println(new Xor[Boolean](0, 0, 0, 0, 0).compile)
  }
}
