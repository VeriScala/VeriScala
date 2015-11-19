package NewHDLExample.FIFO

import NewHDL.Core.HDLBase._

class FIFO[T](clk: HDL[Boolean], rst: HDL[Boolean],
  input: HDL[T], output: HDL[T], width: Int, depth: Int, init: T)
    extends HDLClass {

  def fifo = module {
    sync(clk, 1) {
      val fifo_registers =
        (for (i <- 0 until depth) yield HDLlize(init))
      when (rst) {
        output := 0
        fifo_registers.foreach(_ := 0)
      } .otherwise {
        fifo_registers(0) := input
        output := fifo_registers(depth - 1)
        if (depth > 1)
          for (i <- 0 until depth - 1)
            fifo_registers(i + 1) := fifo_registers(i)
      }
    }
  }

  override val toCompile = List(fifo)
}

object Main {
  def main(args: Array[String]) {
    val depth = 5
    val width = 6
    val handle_FIFO = new FIFO(false, false,
      Unsigned(0, width), Unsigned(0, width),
      width, depth, Unsigned(0, width))
    handle_FIFO.compile.toConsole
  }

}
