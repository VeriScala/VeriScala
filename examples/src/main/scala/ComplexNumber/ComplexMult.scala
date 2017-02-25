/*package NewHDLExample.ComplexNumber.Mult

import NewHDL.Core.HDLBase._

class ComplexMult[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: CHDL[T], b: CHDL[T], z: CHDL[T]) extends HDLClass {

  def complexMult = module {
    sync(clk, 1) {
      z := a * b
    }
  }
  override val toCompile = List(complexMult)
}

object Main {
  def main(args: Array[String]) {
    val handle_Mult = new ComplexMult(false, false,
      ComplexNumber(0, 5, 0, 5), ComplexNumber(1, 5, 2, 5), ComplexNumber(0, 6, 0, 6))
    handle_Mult.compile.toConsole
  }
}*/