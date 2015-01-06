import org.scalatest.FunSuite

import NewHDLExample.Comp.Comparator
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class ComparatorTestBench(clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[Signed], b: HDL[Signed], z: HDL[Signed],
  A: Iterator[Signed], B: Iterator[Signed])
    extends Comparator(clk, rst, a, b, z) with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      rst := b0
      a := A.next()
      b := B.next()
    })

  override val toSimulate = List(comp, bench)
  override val traceFileName = "comp.vcd"
}

class ComparatorTest extends FunSuite {

  val A = List(0, 1, 1, 15, 0, 15, 15).map(Signed(_, 5)).iterator
  val B = List(1, 0, 1, 0, 15, 15, 15).map(Signed(_, 5)).iterator
  val Z = List(0, -1, 1, 0, 1, -1, 0).iterator

  test("test comparator") {
    val clk = HDL(b0)
    val z = HDL(Signed(0, 2))
    val bench = new ComparatorTestBench(clk, b0,
      Signed(0, 5), Signed(0, 5), z, A, B)
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
