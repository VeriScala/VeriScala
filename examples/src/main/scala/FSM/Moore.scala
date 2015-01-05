package NewHDLExample.FSM.Moore

import NewHDL.Core.HDLBase._

class Moore[T](clk: HDL[Boolean], rst: HDL[Boolean],
  cars_green: HDL[Boolean], cars_yellow: HDL[Boolean], cars_red: HDL[Boolean],
  ppl_green: HDL[Boolean], ppl_yellow: HDL[Boolean], ppl_red: HDL[Boolean],
  timerStep: Int)
    extends HDLClass {

  object StatesTrafficLights extends Enumeration {
    type StatesTrafficLights = Value
    val cars_go, cars_stop, people_go, people_stop = Value
  }
  import StatesTrafficLights._

  private val timerStep1 = timerStep + 1

  def moore = module {
    val state_traffic = HDLlize(Unsigned(people_stop.id, 2))
    val timer = HDLlize(Unsigned(1, timerStep1 + 1))

    sync(clk, 1) {
      when (rst) {
        timer := 1
        state_traffic := cars_stop.id
      } .otherwise {
        when (state_traffic is cars_stop.id) {
          timer := 1
          state_traffic := people_go.id
        } .elsewhen (state_traffic is people_go.id) {
          timer := (timer * 2) % math.pow(2, timerStep1).toInt
          when (timer(timerStep) is 1) {
            state_traffic := people_stop.id
          }
        } .elsewhen (state_traffic is people_stop.id) {
          timer := 1
          state_traffic := cars_go.id
        } .elsewhen (state_traffic is cars_go.id) {
          timer := (timer * 2) % math.pow(2, timerStep1).toInt
          when (timer(timerStep) is 1) {
            state_traffic := cars_stop.id
          }
        } .otherwise {
          state_traffic := people_stop.id
        }
      }
    }

    async {
      when (state_traffic is cars_go.id) {
        cars_green := 1
        cars_yellow := 0
        cars_red := 0
        ppl_green := 0
        ppl_yellow := 0
        ppl_red := 1
      } .elsewhen (state_traffic is cars_stop.id) {
        cars_green := 0
        cars_yellow := 1
        cars_red := 0
        ppl_green := 0
        ppl_yellow := 0
        ppl_red := 1
      } .elsewhen (state_traffic is people_go.id) {
        cars_green := 0
        cars_yellow := 0
        cars_red := 1
        ppl_green := 1
        ppl_yellow := 0
        ppl_red := 0
      } .elsewhen (state_traffic is people_stop.id) {
        cars_green := 0
        cars_yellow := 0
        cars_red := 1
        ppl_green := 0
        ppl_yellow := 1
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

  override val toCompile = List(moore)
}

object Main {
  def main(args: Array[String]) {
    println(new Moore(false, false,
      false, false, false,
      false, false, false,
      2).compile)
  }
}
