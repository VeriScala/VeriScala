package NewHDLExample.MultiModule

import NewHDL.Core.HDLBase._
import NewHDLExample.Arithmetic.Add.Adder
import NewHDLExample.Arithmetic.Sub.Subtractor

class AddSub[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], o1: HDL[T], o2: HDL[T]) extends HDLClass {

  val subModule_Add = new Adder(clk, rst, a, b, o1)
  val subModule_Sub = new Subtractor(clk, rst, a, b, o2)

  def addsub = module {
    sync(clk, 1) {
      o1 := 0
      o2 := 0
    }
  }

  override val toCompile = List(addsub.addExternalModule(subModule_Add).addExternalModule(subModule_Sub))
}

object Main {
  def main(args: Array[String]) {
    val handle_addsub = new AddSub(false, false, Signed(0, 5), Signed(1, 5), Signed(0, 6), Signed(0, 6))
    handle_addsub.compile.toConsole
  }
}
