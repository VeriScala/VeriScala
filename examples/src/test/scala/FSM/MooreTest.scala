import org.scalatest.FunSuite

import NewHDLExample.FSM.Moore.Moore
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class MooreTestBench(clk: HDL[Boolean], rst: HDL[Boolean],
  cars_green: HDL[Boolean], cars_yellow: HDL[Boolean], cars_red: HDL[Boolean],
  ppl_green: HDL[Boolean], ppl_yellow: HDL[Boolean], ppl_red: HDL[Boolean],
  timerStep: Int) extends Moore(clk, rst,
    cars_green, cars_yellow, cars_red,
    ppl_green, ppl_yellow, ppl_red, timerStep) with SimulationSuite {
  def bench = module {
    delay(1) {
      clk := ~clk
    }

    sync(clk, 0) {
      rst := b0
    }
  }

  override val toSimulate = List(moore, bench)
  override val traceFileName = "moore.vcd"
}

class MooreTest extends FunSuite {
  test("test moore") {
    val clk = HDL(b0)
    val cars_green = HDL(b0)
    val cars_yellow = HDL(b0)
    val cars_red = HDL(b0)
    val ppl_green = HDL(b0)
    val ppl_yellow = HDL(b0)
    val ppl_red = HDL(b0)
    val bench = new MooreTestBench(clk, b1,
      cars_green, cars_yellow, cars_red,
      ppl_green, ppl_yellow, ppl_red, 2)
    bench since 0 until 40 every 2 run {
      assert(clk === 0)
    }
    bench since 1 until 40 every 2 run {
      assert(clk === 1)
    }

    bench since 1 until 40 every 16 run {
      assert(cars_yellow === 1)
    }
    bench since 3 until 40 every 16 run {
      assert(cars_yellow === 0)
    }
    bench since 3 until 40 every 16 run {
      assert(cars_red === 1)
    }
    bench since 11 until 40 every 16 run {
      assert(cars_red === 0)
    }
    bench since 11 until 40 every 16 run {
      assert(cars_green === 1)
    }
    bench since 17 until 40 every 16 run {
      assert(cars_green === 0)
    }

    bench since 3 until 40 every 16 run {
      assert(ppl_green === 1)
    }
    bench since 9 until 40 every 16 run {
      assert(ppl_green === 0)
    }
    bench since 9 until 40 every 16 run {
      assert(ppl_yellow === 1)
    }
    bench since 11 until 40 every 16 run {
      assert(ppl_yellow === 0)
    }
    bench since 11 until 40 every 16 run {
      assert(ppl_red === 1)
    }
    bench since 19 until 40 every 16 run {
      assert(ppl_red === 0)
    }
    bench test
  }
}
