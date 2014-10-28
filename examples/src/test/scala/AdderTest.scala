import org.scalatest.FunSuite

import NewHDLExample.Arithmetic.Adder

import NewHDL.Core.HDLBase._
import NewHDL.Core.Arith
import NewHDL.Core.ArithCompiler
import NewHDL.Simulation.Simulation

class AdderTestBench[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T])
    extends Adder[T](clk, rst, a, b, z) with Arith with ArithCompiler {

  val A = List(0, 0, 1, 1, 15, 0, 15).iterator
  val B = List(0, 1, 0, 1, 0, 15, 15).iterator

  def bench = module {
    delay(1) {
      clk := ~clk
    }

    sync(clk, 0) {
      rst := 0
      a := A.next()
      b := B.next()
    }
  }
}


class AdderTest extends FunSuite with Simulation {
  test("test adder") {
    simulate(new AdderTestBench(0, 0,
      Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 5)))
  }
}
