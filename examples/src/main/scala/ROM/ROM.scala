package NewHDLExample.ROM

import NewHDL.Core.HDLBase._

class ROM[T](clk: HDL[Boolean], addr: HDL[Unsigned],
  dout: HDL[T], data: HDLValueList[T]) extends HDLClass {

  def rom = module {
    sync(clk, 1) {
      dout := data(addr)
    }
  }

  override val toCompile = List(rom)
}

object Main {
  def main(args: Array[String]) {
    val wl_addr = 2
    val wl_data = 3
    val data_list = List(2, 3, 6, 7).map(Unsigned(_, wl_data))
    println(new ROM(0,
      Unsigned(0, wl_addr), Unsigned(0, wl_data), data_list).compile)
  }
}
