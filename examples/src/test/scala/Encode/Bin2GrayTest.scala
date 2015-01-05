import org.scalatest.FunSuite

import NewHDLExample.Encode.Bin2Gray
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class Bin2GrayTestBench[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  g: HDL[T], b: HDL[T], width: Int, B: Iterator[T])
    extends Bin2Gray[T](clk, rst, g, b, width) with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      b := B.next()
    })

  override val toSimulate = List(encode, bench)
  override val traceFileName = "encode.vcd"
}

class Bin2GrayTest extends FunSuite {
  val width = 4
  val B = (0 until 16).toList.map(Unsigned(_, width)).iterator

  test("test bin2gray") {
    val clk = HDL(false)
    val g = HDL(Unsigned(0, width))
    val bench = new Bin2GrayTestBench(clk, false,
      g, Unsigned(0, width), width, B)
    bench since 0 until 10 every 2 run {
      assert(clk === 0)
    }
    bench since 1 until 10 every 2 run {
      assert(clk === 1)
    }
    bench test
  }
}
