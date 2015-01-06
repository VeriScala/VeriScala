import org.scalatest.FunSuite

import NewHDLExample.Gate.Xor.Xor
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class XorTestBench[T](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T], A: Iterator[T], B: Iterator[T])
    extends Xor[T](clk, rst, a, b, z) with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      rst := b0
      a := A.next()
      b := B.next()
    })

  override val toSimulate = List(xor, bench)
  override val traceFileName = "xor.vcd"
}

class XorTest extends FunSuite {

  implicit def int2bool(x: Int) = if (x > 0) true else false

  val A = List(0, 1, 1, 0, 0).map(int2bool(_)).iterator
  val B = List(1, 0, 1, 0, 0).map(int2bool(_)).iterator
  val Z = List(0, 1, 1, 0, 0).iterator

  test("test xor") {
    val clk = HDL(false)
    val z = HDL(false)
    val bench = new XorTestBench[Boolean](clk, false,
      false, false, z, A, B)
    bench since 0 until 10 every 2 run {
      assert(clk === 0)
    }
    bench since 1 until 10 every 2 run {
      assert(clk === 1)
      assert(z === Z.next)
    }
    bench test
  }
}
