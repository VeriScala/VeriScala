import org.scalatest.FunSuite

import NewHDLExample.Gate.Or.Or
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class OrTestBench[T](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T], A: Iterator[T], B: Iterator[T])
    extends Or[T](clk, rst, a, b, z) with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      rst := 0
      a := A.next()
      b := B.next()
    })

  override val toSimulate = List(or, bench)
  override val traceFileName = "or.vcd"
}

class OrTest extends FunSuite {

  implicit def int2bool(x: Int) = if (x > 0) true else false

  val A = List(0, 0, 1, 1, 0).map(int2bool(_)).iterator
  val B = List(0, 1, 0, 1, 0).map(int2bool(_)).iterator
  val Z = List(0, 1, 1, 1, 0).iterator

  test("test or") {
    val clk = HDL(false)
    val z = HDL(false)
    val bench = new OrTestBench[Boolean](clk, 0, 0, 0, z, A, B)
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
