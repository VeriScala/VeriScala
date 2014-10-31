package NewHDL.Simulation.Core

import NewHDL.Core.HDLClass
import NewHDL.Core.Base
import NewHDL.Core.HDLBase._

trait SimulationBase {
  def exec[T](exp: HDLExp[T]): List[Int]

  val toSimulate: List[HDLModule] = List()

  def simulate { toSimulate.map(simulate(_)) }

  var regs: Set[Register] = Set()

  def simulate(module: HDLModule) = {
    regs = Set()

    for (param <- module.params) {
      regs ++= param.registers
    }

    for (block <- module.blocks) {
      for (exp <- block.exps) {
        exec(exp)
      }
    }
  }
}

trait BasicSimulations extends SimulationBase {
  override def exec[T](exp: HDLExp[T]): List[Int] = {
    exp match {
      case HDLWhen(cond, suc, fal) =>
        val c = cond match {
          case cr: HDLReg[T] => cr.value > 1
        }
        if (c) suc.flatMap(exec(_)).toList
        else fal.flatMap(exec(_)).toList
      case HDLAssign(lhs, rhs) =>
        val res = exec(rhs)
        val iter = res.iterator
        lhs.registers.map { reg =>
          reg.next = iter.next
        }
        res
      case r: HDLReg[T] =>
        r.registers.map(_.value).toList
    }
  }
}
