/*package NewHDLExample.Inherit.Parent

import NewHDL.Core.HDLBase._

class Parent[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T]) extends HDLClass {
  def add = module {
    sync(clk, 1) {
      when (rst) {
        z := 0
      } .otherwise {
        z := a + b
      }
    }
  }

  override val toCompile = List(add)
}

object Main {
  def main(args: Array[String]) {
    val handle_Adder = new Parent(false, false,
      Signed(0, 5), Signed(1, 5), Signed(0, 6))
    handle_Adder.compile.toConsole
  }
}*/