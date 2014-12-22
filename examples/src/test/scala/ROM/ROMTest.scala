import org.scalatest.FunSuite

import NewHDLExample.ROM.ROM
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class ROMTestBench[T](clk: HDL[Boolean], addr: HDL[Unsigned],
  dout: HDL[T], data: HDLValueList[T], addrs: Iterator[Unsigned])
    extends ROM(clk, addr, dout, data) with SimulationSuite {

  def bench = module (
    delay(1) {
      clk := ~clk
    },

    delay(4) {
      addr := addrs.next()
    })

  override val toSimulate = List(rom, bench)
  override val traceFileName = "rom.vcd"
}

class ROMTest extends FunSuite {

  val wl_addr = 2
  val wl_data = 3
  val data_list = List(2, 3, 6, 7).map(Unsigned(_, wl_data))
  val addrs = List(1, 2, 3).map(Unsigned(_, wl_addr)).iterator
  val douts = List(2, 2, 3, 3, 6, 6, 7, 7).iterator

  test("test rom") {
    val clk = HDL(false)
    val addr = HDL(Unsigned(0, wl_addr))
    val dout = HDL(Unsigned(0, wl_data))
    val bench = new ROMTestBench(clk, addr, dout, data_list, addrs)
    bench since 0 until 16 every 2 run {
      assert(clk === 0)
    }
    bench since 1 until 16 every 2 run {
      assert(clk === 1)
      assert(dout === douts.next())
    }
    bench test
  }
}
