package NewHDLExample.Inherit.Child

import NewHDL.Core.HDLBase._
import NewHDLExample.Inherit.Parent.Parent

// Not Complete
class Child[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T], ex: HDL[T]) extends Parent(clk, rst, a, b, ex) {

  override val toCompile = List(add)
}

object Main {
  def main(args: Array[String]) {
    val handle_Adder = new Child(false, false,
      Signed(0, 5), Signed(1, 5), Signed(0, 6), Signed(0, 6))
    handle_Adder.compile.toConsole
  }
}