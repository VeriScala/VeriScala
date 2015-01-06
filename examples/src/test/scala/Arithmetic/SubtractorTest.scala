import org.scalatest.FunSuite

import NewHDLExample.Arithmetic.Sub.Subtractor
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class SubtractorTestBench[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T], A: Iterator[T], B: Iterator[T])
    extends Subtractor[T](clk, rst, a, b, z) with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      rst := b0
      a := A.next()
      b := B.next()
    })

  override val toSimulate = List(sub, bench)
  override val traceFileName = "sub.vcd"
}

class SubTest extends FunSuite {

  val A = List(0, 1, 1, 15, 0, 15, 15).map(Unsigned(_, 4)).iterator
  val B = List(1, 0, 1, 0, 15, 15, 15).map(Unsigned(_, 4)).iterator
  val Z = List(0, 31, 1, 0, 15, 17, 0).iterator

  test("test sub") {
    val clk = HDL(false)
    val z = HDL(Unsigned(0, 5))
    val bench = new SubtractorTestBench(clk, false,
      Unsigned(0, 4), Unsigned(0, 4), z, A, B)
    bench since 0 until 14 every 2 run {
      assert(clk === 0)
    }
    bench since 1 until 14 every 2 run {
      assert(clk === 1)
      assert(z === Z.next)
    }
    bench test
  }
}
