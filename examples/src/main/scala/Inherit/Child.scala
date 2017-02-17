/*package NewHDLExample.Inherit.Child

import NewHDL.Core.HDLBase._
import NewHDLExample.Inherit.Parent.Parent

class Child[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], ex: HDL[T], d: HDL[T]) extends Parent(clk, rst, a, b, d) {

  def sub = module {
    sync(clk, 1) {
      when (rst) {
        ex := 0
      } .otherwise {
        ex := a + b
      }
    }
  }

  override val toCompile = List(add, sub)
}

object Main {
  def main(args: Array[String]) {
    val handle_Adder = new Child(false, false,
      Signed(0, 5), Signed(1, 5), Signed(0, 6), Signed(0, 6))
    handle_Adder.compile.toConsole
  }
}*/