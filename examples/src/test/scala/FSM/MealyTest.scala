import org.scalatest.FunSuite

import NewHDLExample.FSM.Mealy.Mealy
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Core.SimulationSuite

class MealyTestBench(clk: HDL[Boolean], rst: HDL[Boolean],
  car_waiting: HDL[Boolean], ppl_waiting: HDL[Boolean],
  cars_green: HDL[Boolean], cars_yellow: HDL[Boolean], cars_red: HDL[Boolean],
  ppl_green: HDL[Boolean], ppl_yellow: HDL[Boolean], ppl_red: HDL[Boolean],
  car_waiting_list: Iterator[Boolean], ppl_waiting_list: Iterator[Boolean])
    extends Mealy(clk, rst,
      car_waiting, ppl_waiting,
      cars_green, cars_yellow, cars_red,
      ppl_green, ppl_yellow, ppl_red) with SimulationSuite {
  def bench = module {
    delay(1) {
      clk := ~clk
    }

    sync(clk, 0) {
      rst := b0
      car_waiting := car_waiting_list.next()
      ppl_waiting := ppl_waiting_list.next()
    }
  }

  override val toSimulate = List(mealy, bench)
  override val traceFileName = "mealy.vcd"
}

class MealyTest extends FunSuite {
  test("test mealy") {

    val cl = List(b0, b0, b1, b0).iterator
    val pl = List(b0, b1, b0, b0).iterator

    val clk = HDL(b0)
    val car_waiting = HDL(b0)
    val ppl_waiting = HDL(b0)
    val cars_green = HDL(b0)
    val cars_yellow = HDL(b0)
    val cars_red = HDL(b0)
    val ppl_green = HDL(b0)
    val ppl_yellow = HDL(b0)
    val ppl_red = HDL(b0)
    val bench = new MealyTestBench(clk, b1,
      car_waiting, ppl_waiting,
      cars_green, cars_yellow, cars_red,
      ppl_green, ppl_yellow, ppl_red, cl, pl)
    bench since 0 until 7 every 2 run {
      assert(clk === 0)
    }
    bench since 1 until 7 every 2 run {
      assert(clk === 1)
    }
    bench since 1 until 4 every 1 run {
      assert(cars_green === 1)
    }
    bench since 4 until 7 every 1 run {
      assert(cars_green === 0)
    }
    bench since 5 until 7 every 1 run {
      assert(cars_red === 1)
    }
    bench since 4 until 5 every 1 run {
      assert(cars_yellow === 1)
    }

    bench since 1 until 5 every 1 run {
      assert(ppl_red === 1)
    }
    bench since 5 until 7 every 1 run {
      assert(ppl_red === 0)
    }
    bench since 5 until 6 every 1 run {
      assert(ppl_green === 1)
    }
    bench since 6 until 7 every 1 run {
      assert(ppl_yellow === 1)
    }
    bench test
  }
}
