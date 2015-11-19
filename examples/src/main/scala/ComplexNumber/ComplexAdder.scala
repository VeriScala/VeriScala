package ComplexNumber

import NewHDL.Core.HDLBase._


class ComplexAdder[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: CHDL[T], b: CHDL[T], z: CHDL[T]) extends HDLClass {

  def complexAdd = module {
    sync(clk, 1) {
      z := a + b
    }
  }
  override val toCompile = List(complexAdd)
}

object Main {
  def main(args: Array[String]) {
    val handle_Adder = new ComplexAdder(false, false,
      ComplexNumber(0, 5, 0, 5), ComplexNumber(1, 5, 2, 5), ComplexNumber(0, 6, 0, 6))
    println(handle_Adder.compile)
  }
}