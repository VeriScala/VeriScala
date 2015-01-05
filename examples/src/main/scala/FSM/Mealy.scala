package NewHDLExample.FSM.Mealy

import NewHDL.Core.HDLBase._

object StatesTrafficLights extends Enumeration {
  type StatesTrafficLights = Value
  val cars_go, people_go = Value
}

import StatesTrafficLights._

class Mealy[T](clk: HDL[Boolean], rst: HDL[Boolean],
  car_waiting: HDL[Boolean], ppl_waiting: HDL[Boolean],
  cars_green: HDL[Boolean], cars_yellow: HDL[Boolean], cars_red: HDL[Boolean],
  ppl_green: HDL[Boolean], ppl_yellow: HDL[Boolean], ppl_red: HDL[Boolean])
    extends HDLClass {
  def mealy = module {
    val state_traffic = HDLlize(false)

    sync(clk, 1) {
      when (rst) {
        state_traffic := cars_go.id
      } .otherwise {
        when (state_traffic is cars_go.id) {
          when (ppl_waiting) {
            state_traffic := people_go.id
          }
        } .elsewhen (state_traffic is people_go.id) {
          when (car_waiting) {
            state_traffic := cars_go.id
          }
        } .otherwise {
          state_traffic := cars_go.id
        }
      }
    }

    async {
      when (state_traffic is cars_go.id) {
        cars_green := ~ppl_waiting
        cars_yellow := ppl_waiting
        cars_red := 0
        ppl_green := 0
        ppl_yellow := 0
        ppl_red := 1
      } .elsewhen (state_traffic is people_go.id) {
        cars_green := 0
        cars_yellow := 0
        cars_red := 1
        ppl_green := ~car_waiting
        ppl_yellow := car_waiting
        ppl_red := 0
      } .otherwise {
        cars_green := 0
        cars_yellow := 0
        cars_red := 1
        ppl_green := 0
        ppl_yellow := 0
        ppl_red := 1
      }
    }
  }

  override val toCompile = List(mealy)
}

object Main {
  def main(args: Array[String]) {
    println(new Mealy(false, false,
      false, false,
      false, false, false,
      false, false, false).compile)
  }
}
