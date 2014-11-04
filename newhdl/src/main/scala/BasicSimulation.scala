package NewHDL.Simulation.Core

import NewHDL.Core.HDLClass
import NewHDL.Core.Base
import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Waiter
import NewHDL.Simulation.SyncWaiter
import NewHDL.Simulation.AsyncWaiter
import NewHDL.Simulation.DelayWaiter
import NewHDL.Simulation.Exceptions.SimulatorException

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Date
import java.util.Locale
import java.text.SimpleDateFormat

import scala.collection.mutable.PriorityQueue


trait SimulationBase {

  def exec[T](exp: HDLExp[T]): List[Int]

  val toSimulate: List[HDLModule] = List()

  val traceFileName: String = null

  object trace {
    var nameMap: Map[Register, String] = Map()
    var tracing: Boolean = false
    var file: File = null
    var writer: BufferedWriter = null

    def start(fileName: String) {
      nameMap = regs.zip((0 until regs.size).map("N" + _.toString)).toMap

      file = new File(fileName)

      writer = new BufferedWriter(new FileWriter(file))

      log(List(
        "$date",
        new SimpleDateFormat("    EEE MMM dd HH:mm:ss yyyy", Locale.UK).format(
          new Date()
        ),
        "$end",
        "$version",
        "    ScalaHDL 0.0.1",
        "$end",
        "$timescale",
        "    1ns",
        "$end"
      ).mkString("\n"))

      log("\n$scope module main $end")
      for (kv <- nameMap)
        log("$var reg %d %s %s $end".format(kv._1.length, kv._2, kv._1.name))
      for (mod <- toSimulate)
      {
        log("$scope module %s $end".format(mod.name))
        val params = mod.params
        for (param <- params) {
          for (reg <- param.registers) {
            log("$var reg %d %s %s $end".format(reg.length,
              nameMap(reg), reg.name))
          }
        }
        log("$upscope $end")
      }
      log("$upscope $end")
      log("\n$enddefinitions $end")
      log("$dumpvars")
      for (reg <- regs) logNew(reg)
      log("$end")
    }

    def logNew(reg: Register) {
      if (file != null && writer != null && nameMap.contains(reg))
        log("b%s %s".format(reg.value.toBinaryString, nameMap(reg)))
    }

    def stop() {
      if (file != null && writer != null) {
        writer.flush()
        writer.close()
        file = null
        writer = null
      }
    }

    def log(s: String) {
      if (file != null && writer != null) {
        writer.write(s)
        writer.newLine()
      }
    }
  }

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
    }
  }

  protected def doSimulation(maxTime: Int): Int = {
    if (maxTime == 0) return currentTime
    while (true) {
      for (reg <- regs) {
        val old = reg.value
        waiters = reg.update ::: waiters
        if (reg.value != old)
          trace.logNew(reg)
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
        if (futureEvents.isEmpty) {
          trace.log("#" + maxTime)
          return currentTime
        }
        val spans = futureEvents.span(_._1 == futureEvents.head._1)
        val events = spans._1
        futureEvents = spans._2
        nextTime = events.head._1
        waiters = events.map(_._2).toList
        trace.log("#" + nextTime)
        if (nextTime > maxTime) return currentTime
        if (events.isEmpty) return currentTime
      }
    }
    currentTime
  }

  def simulate(maxTime: Int) {
    if (isStarted)
      throw SimulatorException("Simulator is already running!")
    if (traceFileName != null)
    startSimulate
    trace.start(traceFileName)
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
    trace.stop
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
        lhs.registers.zip(res).map { kv =>
          kv._1.next = kv._2
        }
        res
      case HDLRev(x) =>
        List(if (exec(x)(0) > 0) 0 else 1)
      case HDLAdd(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 + tuple._2)
        r
      case r: HDLReg[T] =>
        r.registers.map(_.value).toList
    }
  }
}

trait SimulationSchedule { this: SimulationBase =>
  var tasks: PriorityQueue[(Int, () => Unit)] =
    new PriorityQueue[(Int, () => Unit)]()(Ordering[(Int)].on(x => -x._1))

  case class TestStartPoint(time: Int) {
    def to(endTime: Int) = TestDuration(time, endTime)
    def until(endTime: Int) = TestDuration(time, endTime - 1)
  }

  case class TestDuration(startTime: Int, endTime: Int) {
    def every(duration: Int) =  TestSchedule(startTime, endTime, duration)
  }

  case class TestSchedule(startTime: Int, endTime: Int, duration: Int) {
    def run(f: => Unit) {
      (startTime to endTime by duration).foreach { t =>
        tasks enqueue ((t, () => f))
      }
    }
  }

  case class TestTask(s: TestSchedule, f: () => Unit)

  def since(time: Int): TestStartPoint = TestStartPoint(time)

  def test() {
    simulate(0)
    var now = 0
    while (!tasks.isEmpty) {
      val (time, task) = tasks.dequeue
      if (time > now)
        continue(time - now)
      task()
      now = time
    }
    stop
  }
}

trait SimulationSuite extends BasicSimulations with SimulationSchedule
