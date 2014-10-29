package NewHDL.Simulation

import NewHDL.Core.HDLClass
import NewHDL.Core.Base
import NewHDL.Core.HDLBase

trait Simulation extends Base {
  import HDLBase._

  def simulate(ms: HDLClass) {
    new Simulator(ms)
  }
}

class Simulator(hdl: HDLClass) {
  for (module <- hdl.toSimulate)
    for (param <- module.params)
      println(param.registers)
}
