package Camera.RAW2RGB

import NewHDL.Core.HDLBase._

class RAW2RGB(oRed:HDL[Unsigned],oGreen:HDL[Unsigned],oBlue:HDL[Unsigned],oDVAL:HDL[Boolean],
               iX_Cont:HDL[Unsigned],iY_Cont:HDL[Unsigned],iDATA:HDL[Unsigned],
               iDVAL:HDL[Boolean],iCLK:HDL[Boolean],iRST:HDL[Boolean]) extends HDLClass{



  def RAW2RGB = module {

    val mDATA_0 = HDLlize(Unsigned(0,12))
    val mDATA_1 = HDLlize(Unsigned(0,12))
    val mDATAd_0 = HDLlize(Unsigned(0,12))
    val mDATAd_1 = HDLlize(Unsigned(0,12))
    val mCCD_R = HDLlize(Unsigned(0,12))
    val mCCD_G = HDLlize(Unsigned(0,13))
    val mCCD_B = HDLlize(Unsigned(0,12))
    val mDVAL = HDLlize(b0)

    sync((iCLK,1),(iRST,0)){
      when(iRST isnot 1) {
        mCCD_R := 0
        mCCD_G := 0
        mCCD_B := 0
        mDATAd_0 := 0
        mDATAd_1 := 0
        mDVAL := 0
      }.otherwise {
        mDATAd_0 := mDATA_0
        mDATAd_1 := mDATA_1
        when ((iY_Cont(0) is 1) | (iX_Cont(0) is 1)){
          mDVAL := 0
        }.otherwise{
          mDVAL := iDVAL
        }
        when ( (iY_Cont(0)~~iX_Cont(0)) is 2) {
          mCCD_R := mDATA_0
          mCCD_G := mDATAd_0 + mDATA_1
          mCCD_B := mDATAd_1
        }.elsewhen ((iY_Cont(0)~~ iX_Cont(0)) is 3) {
          mCCD_R := mDATAd_0
          mCCD_G := mDATA_0 + mDATAd_1
          mCCD_B := mDATA_1
        }.elsewhen ((iY_Cont(0)~~ iX_Cont(0)) is 0){
          mCCD_R := mDATA_1
          mCCD_G := mDATA_0 + mDATAd_1
          mCCD_B := mDATAd_0
        }.elsewhen (iY_Cont(0) ~~ iX_Cont(0) is 1) {
          mCCD_R := mDATAd_1
          mCCD_G := mDATAd_0 + mDATA_1
          mCCD_B := mDATA_0
        }.otherwise{}
      }
    }
  }
  override val toCompile = List(RAW2RGB)

}

object Main{
  def main(args: Array[String]): Unit ={
    println(new RAW2RGB(Unsigned(0,12),Unsigned(0,12),Unsigned(0,12),b0,Unsigned(0,11),
    Unsigned(0,11),Unsigned(0,12),b0,b0,b0).compile)
  }
}