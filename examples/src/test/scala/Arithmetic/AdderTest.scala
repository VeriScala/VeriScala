import org.scalatest.FunSuite

import NewHDLExample.Arithmetic.Add.Adder
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class AdderTestBench[T <: Arithable](clk: HDL[Boolean], rst: HDL[Boolean],
  a: HDL[T], b: HDL[T], z: HDL[T], A: Iterator[T], B: Iterator[T])
    extends Adder[T](clk, rst, a, b, z) with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    sync(clk, 0) {
      rst := b0
      a := A.next()
      b := B.next()
    })

  override val toSimulate = List(add, bench)
  override val traceFileName = "adder.vcd"
}

class AdderTest extends FunSuite {

  val A = List(0, 1, 1, 15, 0, 15, 15).map(Unsigned(_, 4)).iterator
  val B = List(1, 0, 1, 0, 15, 15, 15).map(Unsigned(_, 4)).iterator
  val Z = List(0, 1, 1, 2, 15, 15, 30).iterator

  test("test adder") {
    val clk = HDL(b0)
    val z = HDL(Unsigned(0, 5))
    val bench = new AdderTestBench(clk, b0,
      Unsigned(0, 4), Unsigned(0, 4), z, A, B)


    // open FPGA(Simulator) network host and client
    // 8081 is FPGA(Simulator) port, 8082 is ScalaHDL(bench) host port
    bench.udpcore_on_off = true
    new Thread(new Runnable {
      def run() {
        bench.udpcore_debug_run("59.78.56.59", 8082, 8081)
      }
    }).run()


    // open ScalaHDL network host and client
    // 8081 is FPGA(Simulator) port, 8082 is ScalaHDL(bench) host port
    bench.network_on_off = true
    new Thread(new Runnable {
      def run() {
        bench.network_debug_run("59.78.56.59", 8081, 8082)
      }
    }).run()


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
