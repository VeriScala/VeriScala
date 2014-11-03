package NewHDL.Simulation.Core

import NewHDL.Core.HDLClass
import NewHDL.Core.Base
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Waiter
import NewHDL.Simulation.SyncWaiter
import NewHDL.Simulation.AsyncWaiter
import NewHDL.Simulation.DelayWaiter
import NewHDL.Simulation.Exceptions.SimulatorException

import scala.collection.mutable.PriorityQueue

trait SimulationBase {

  def exec[T](exp: HDLExp[T]): List[Int]

  val toSimulate: List[HDLModule] = List()

  protected var regs: Set[Register] = Set()
  protected var futureEvents: PriorityQueue[(Int, Waiter)] =
    new PriorityQueue[(Int, Waiter)]()(Ordering[(Int)].on(x => -x._1))
  protected var currentTime = 0
  protected var nextTime = 0
  protected var waiters: List[Waiter] = List()
  protected var isStarted: Boolean = false

  protected def startSimulate {
    waiters = List()
    isStarted = true
    currentTime = 0
    nextTime = 0
    toSimulate.map(startSimulate(_))
  }

  protected def startSimulate(module: HDLModule) {

    for (param <- module.params) {
      regs ++= param.registers
    }

    for (block <- module.blocks) block match {
      case s: HDLSyncBlock =>
        val w = new SyncWaiter(s)
        w.reg.registers map { register =>
          register.addWaiter(w, w.when)
        }
        waiters = w :: waiters
      case a: HDLAsyncBlock =>
        val w = new AsyncWaiter(a)
        w.senslist map { hdlreg =>
          hdlreg.registers map { register =>
            register.addWaiter(w)
          }
        }
        waiters = w :: waiters
      case d: HDLDelayBlock =>
        val w = new DelayWaiter(d, d.duration)
        futureEvents enqueue ((d.duration, w))
        waiters = w :: waiters
    }
  }

  protected def doSimulation(maxTime: Int): Int = {
    if (maxTime == 0) return currentTime
    while (true) {
      for (reg <- regs) {
        waiters = reg.update ::: waiters
      }
      waiters = waiters.distinct
      for (waiter <- waiters) {
        val exps = waiter.next
        for (exp <- exps) {
          exec(exp)
        }
        waiter match {
          case d: DelayWaiter =>
            futureEvents enqueue ((nextTime + d.duration, waiter))
          case _ => ()
        }
      }
      currentTime = nextTime
      waiters = List()
      if (!regs.exists(_.needUpdate)) {
        if (futureEvents.isEmpty)
          return currentTime
        val spans = futureEvents.span(_._1 == futureEvents.head._1)
        val events = spans._1
        futureEvents = spans._2
        nextTime = events.head._1
        waiters = events.map(_._2).toList
        if (nextTime > maxTime) return currentTime
        if (events.isEmpty) return currentTime
      }
    }
    currentTime
  }

  def simulate(maxTime: Int) {
    if (isStarted)
      throw SimulatorException("Simulator is already running!")
    startSimulate
    doSimulation(maxTime)
  }

  def continue(maxTime: Int) {
    if (!isStarted)
      throw SimulatorException("Simulator has not been started!")
    doSimulation(maxTime)
  }

  def stop {
    if (!isStarted)
      throw SimulatorException("Simulator has not been started!")
    isStarted = false
    currentTime = 0
    nextTime = 0
    waiters = List()
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
