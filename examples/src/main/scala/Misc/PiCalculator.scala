package NewHDLExample.Misc.PiCalculator

import NewHDL.Core.HDLBase._

class Calc(clk: HDL[Boolean], rst: HDL[Boolean],
  result: HDL[Unsigned], x: HDL[Unsigned], y: HDL[Unsigned],
  count: List[HDL[Unsigned]], n: Int) extends HDLClass {

  val d = n / count.size

  def calc = module {
    sync(clk, 1) {
      when (rst) {
        x := 1
        y := 1
        for (c <- count) {
          c := 0
        }
        result := 0
      } .elsewhen (y <= d) {
        val cs = count.zipWithIndex
        for ((c, i) <- cs; offset = u(i * d)) {
          when (x * x + (y + offset) * (y + offset) <= n * n) {
            c := c + 1
          }
        }
        when (x == n) {
          x := 1
          y := y + 1
        } .otherwise {
          x := x + 1
        }
      } .otherwise {
        result := count.foldLeft(HDLAdd(u(0), u(0)))((a, b) => a + b)
      }
    }
  }

  override val toCompile = List(calc)
}

object Main {
  def main(args: Array[String]) {
    val width = 32
    println((new Calc(b0, b0,
      Unsigned(0, width), Unsigned(0, width), Unsigned(0, width),
      List(Unsigned(0, width), Unsigned(0, width)),
      math.pow(2, 11).toInt)).compile)
  }
}
