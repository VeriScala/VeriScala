package NewHDLExample.Encode

import NewHDL.Core.HDLBase._

class Bin2Gray[T <: Arithable]( clk: HDL[Boolean], rst: HDL[Boolean],
  g: HDL[T], b: HDL[T], width: Int) extends HDLClass {
  def encode = module {
    async {
      for (i <- 0 until width)
        g(i) := b(i + 1) ^ b(i)
      g(width) := b(width)
    }
  }

  override val toCompile = List(encode)
}

object Main {
  def main(args: Array[String]) {
    val width = 3
    println((new Bin2Gray(false, false,
      Unsigned(0, width + 1), Unsigned(0, width + 1), width)).compile)
  }
}
