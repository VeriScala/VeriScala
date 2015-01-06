import org.scalatest.FunSuite

import NewHDLExample.FIFO.FIFO
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class FIFOTestBench[T](clk: HDL[Boolean], rst: HDL[Boolean],
  input: HDL[T], output: HDL[T], width: Int, depth: Int, init: T,
  inputs: Iterator[T])
    extends FIFO[T](clk, rst, input, output, width, depth, init)
    with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      input := inputs.next()
    })

  override val toSimulate = List(fifo, bench)
  override val traceFileName = "fifo.vcd"
}

class FIFOTest extends FunSuite {
  val width = 3
  val depth = 2
  val inputs = List(7, 0, 3, 0, 1, 0, 0).map(Unsigned(_, width)).iterator
  val outputs = List(0, 0, 0, 7, 0, 3, 0, 1).iterator

  test("test fifo") {
    val clk = HDL(false)
    val input = HDL(Unsigned(0, width))
    val output = HDL(Unsigned(0, width))
    val bench = new FIFOTestBench(clk, false,
      input, output, width, depth, Unsigned(0, width), inputs)
    bench since 0 until 16 every 2 run {
      assert(clk === 0)
    }
    bench since 1 until 16 every 2 run {
      assert(clk === 1)
      assert(output === outputs.next())
    }
    bench test
  }
}
