package Camera

import NewHDL.Core.HDLBase._

class SEG7LUT(oSEG:HDL[Unsigned],iDIG:HDL[Unsigned]) extends HDLClass{

  def SEG7_LUT = module {
    async{
      when (iDIG is 0x1){
        oSEG := 0x79
      }.elsewhen(iDIG is 0x2){
        oSEG := 0x24
      }.elsewhen(iDIG is 0x3){
        oSEG := 0x30
      }.elsewhen(iDIG is 0x4){
        oSEG := 0x19
      }.elsewhen(iDIG is 0x5){
        oSEG := 0x12
      }.elsewhen(iDIG is 0x6){
        oSEG := 0x02
      }.elsewhen(iDIG is 0x7){
        oSEG := 0x78
      }.elsewhen(iDIG is 0x8){
        oSEG := 0x00
      }.elsewhen(iDIG is 0x9){
        oSEG := 0x18
      }.elsewhen(iDIG is 0xa){
        oSEG := 0x08
      }.elsewhen(iDIG is 0xb){
        oSEG := 0x03
      }.elsewhen(iDIG is 0xc){
        oSEG := 0x46
      }.elsewhen(iDIG is 0xd){
        oSEG := 0x21
      }.elsewhen(iDIG is 0xe){
        oSEG := 0x06
      }.elsewhen(iDIG is 0xf){
        oSEG := 0x0e
      }.otherwise{
        oSEG := 0x40
      }
    }
  }
  override val toCompile = List(SEG7_LUT)
}


object Main{
  def main(args: Array[String]): Unit ={
    println(new SEG7LUT(Unsigned(0,7),Unsigned(0,4)).compile)
  }
}
