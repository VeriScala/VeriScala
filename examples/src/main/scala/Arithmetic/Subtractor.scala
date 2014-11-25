package NewHDLExample.Arithmetic.Sub

import NewHDL.Core.HDLBase._

class Subtractor[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T]) extends HDLClass {
  def sub = module {
    sync(clk, 1) {
      when (rst) {
        z := 0
      } .otherwise {
        z := a - b
      }
    }
  }

  override val toCompile = List(sub)
}

object Main {
  def main(args: Array[String]) {
    println((new Subtractor(0, 0,
      Signed(0, 5), Signed(1, 5), Signed(0, 6))).compile)
  }
}
