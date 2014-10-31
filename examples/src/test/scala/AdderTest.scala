import org.scalatest.FunSuite

import NewHDLExample.Arithmetic.Adder

import NewHDL.Core.HDLBase._
import NewHDL.Core.Arith
import NewHDL.Core.ArithCompiler
import NewHDL.Core.ArithSimulations

class AdderTestBench[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T])
    extends Adder[T](clk, rst, a, b, z)
    with Arith with ArithCompiler with ArithSimulations {

  val A = List(0, 0, 1, 1, 15, 0, 15).iterator
  val B = List(0, 1, 0, 1, 0, 15, 15).iterator

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      rst := 0
      a := A.next()
      b := B.next()
    }
  )

  override val toSimulate = List(add, bench)
}


class AdderTest extends FunSuite {
  test("test adder") {
    new AdderTestBench(0, 0,
      Unsigned(0, 4), Unsigned(0, 4), Unsigned(0, 5)).simulate
  }
}
