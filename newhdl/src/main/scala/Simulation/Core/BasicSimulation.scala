package NewHDL.Simulation.Core

import NewHDL.Core.HDLBase._
import NewHDL.Simulation.Exceptions.SimulatorException

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Date
import java.util.Locale
import java.text.SimpleDateFormat
import math.pow

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
  protected var tempRegs: Set[Register] = Set()
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
    for (reg <- module.internalRegs) {
      regs ++= reg.registers
    }

    for (block <- module.blocks) block match {
      case s: HDLSyncBlock =>
        val w = new SyncWaiter(s)
        w.reg.registers map { register =>
          register.addWaiter(w, w.when)
        }
        //waiters = w :: waiters
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
      for (reg <- (regs ++ tempRegs)) {
        reg match {
          case b: RegisterBit =>
            val r = b.getReg
            val old = r.value
            waiters = reg.update ::: waiters
            if (r.value != old)
              trace.logNew(r)
          case r: Register =>
            val old = reg.value
            waiters = reg.update ::: waiters
            if (reg.value != old)
              trace.logNew(reg)
        }
      }
      tempRegs = Set()
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
      if (!(regs ++ tempRegs).exists(_.needUpdate)) {
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
    doSimulation(currentTime + maxTime)
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
  // TODO: change List[Int] to an abstract class?
  override def exec[T](exp: HDLExp[T]): List[Int] = {
    exp match {
      case HDLWhen(conditions) =>
        var res: List[Int] = List()
        conditions.reverse.exists { cond =>
          cond match {
            case HDLNormalCondition(c, f) if (exec(c)(0) > 0)=>
              res = f.flatMap(exec(_)).toList
              true
            case HDLBooleanCondition(b, f) if (b) =>
              res = f.flatMap(exec(_)).toList
              true
            case _ =>
              false
          }
        }
        res
      case HDLEquals(lhs, rhs) =>
        if (exec(lhs) == exec(rhs)) List(1) else List(0)
      case HDLAssign(lhs, rhs) =>
        val res = exec(rhs)
        lhs match {
          case lreg: HDLReg[T] =>
            lreg.registers.zip(res).map { kv =>
              kv._1.setNext(kv._2)
            }
          case lidx: HDLIndex[T] =>
            lidx.registers.zip(res).map { kv =>
              tempRegs = tempRegs + kv._1
              kv._1.setNext(kv._2)
            }
        }
        res
      case HDLRev(x) =>
        List(if (exec(x)(0) > 0) 0 else 1)
      case HDLAdd(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 + tuple._2)
        r
      case HDLSub(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 - tuple._2)
        r
      case HDLMul(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 * tuple._2)
        r
      case HDLDiv(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 / tuple._2)
        r
      case HDLMod(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 % tuple._2)
        r
      case HDLBitwiseAnd(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 & tuple._2)
        r
      case HDLBitwiseOr(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 | tuple._2)
        r
      case HDLBitwiseXor(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 ^ tuple._2)
        r
      case HDLGreaterThan(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 > tuple._2).map(b => if (b) 1 else 0)
        r
      case HDLLessThan(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 < tuple._2).map(b => if (b) 1 else 0)
        r
      case HDLGreaterThanOrEqual(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 >= tuple._2).map(b => if (b) 1 else 0)
        r
      case HDLLessThanOrEqual(x, y) =>
        val p = exec(x).zip(exec(y))
        val r = p.map(tuple => tuple._1 <= tuple._2).map(b => if (b) 1 else 0)
        r
      case HDLIndex(obj, idx) =>
        exec(obj).map(x => (x >> idx) & 1).toList
      case HDLValueListElem(lst, idx) =>
        // is it wise to use index 0?
        exec(lst.lst(exec(idx)(0)))
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
