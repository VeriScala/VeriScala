package Camera.ResetDelay

import NewHDL.Core.HDLBase._

class ResetDelay(iCLK:HDL[Boolean],iRST:HDL[Boolean],oRST_0:HDL[Boolean],oRST_1:HDL[Boolean],
                  oRST_2:HDL[Boolean]) extends HDLClass {


  def Reset_Delay = module {
    val cont = HDLlize(Unsigned(0,32))

    sync((iCLK,1),(iRST,0)) {
      when (iRST is 0) {
        cont := 0
        oRST_0 := 0
        oRST_1 := 0
        oRST_2 := 0
      }.otherwise {
        when (cont isnot 0x11FFFFF) {
          cont := cont + 1
        }
        when (cont >= 0x1FFFFF) {
          oRST_0 := 1
        }
        when (cont >= 0x2FFFFF) {
          oRST_1 := 1
        }
        when (cont >= 0x11FFFFF) {
          oRST_2 := 1
        }
      }
    }
  }
  override val toCompile = List(Reset_Delay)
}

object Main{
  def main(args: Array[String]): Unit ={
    println(new ResetDelay(b0,b0,b0,b0,b0).compile)
  }
}
