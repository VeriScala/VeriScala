package NewHDLExample.RAM

import NewHDL.Core.HDLBase._

class RAM[T](clk: HDL[Boolean], run: HDL[Boolean],we: HDL[Boolean],
  addr: HDL[Unsigned], din: HDL[T], dout: HDL[T], ready: HDL[Boolean],
  depth: Int, width: Int) extends HDLClass {
  def ram = module {
    val mem = HDLlize((1 to depth).map(Unsigned(_, width)))
    sync(clk, 1) {
      when (we) {
        mem(addr) := din
      }
      dout := mem(addr)
    }
  }

  override val toCompile = List(ram)
  override val interface = List(("clk",1),("run",1),("we",1),("addr",(math.log(depth)/math.log(2)).toInt),("din",width),("dout",width),("ready",1))
}

object Main {
  def main(args: Array[String]) {
    val wl_addr = 2
    val wl_data = 3

    val depth = math.pow(2, wl_addr).toInt
    val width = math.pow(2, wl_data).toInt
    val mod = new RAM(false, false, false,
      Unsigned(0, wl_addr), Unsigned(0, wl_data), Unsigned(0, wl_data), false,
      depth, width)
    println("#######################")
    println(mod.compileConfig)
    println("#######################")
    println(mod.compile)
    println("#######################")
    println(mod.compileDebug)
    println("#######################")
    println(mod.compileInvoke)
  }
}
