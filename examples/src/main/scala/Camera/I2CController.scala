package Camera.I2CController

import NewHDL.Core.HDLBase._


class I2CController(CLOCK:HDL[Boolean],I2C_SCLK:HDL[Boolean],I2C_SDAT:HDL[Boolean],
                     I2C_DATA:HDL[Unsigned],GO:HDL[Boolean],END:HDL[Boolean],
                     ACK:HDL[Boolean],RESET:HDL[Boolean]) extends HDLClass{

  def I2CController = module {

    val SDO = HDLlize(b0)
    val SCLK = HDLlize(b0)
    val SD = HDLlize(Unsigned(0,32))
    val SD_COUNTER = HDLlize(Unsigned(0,7))

    val ACK1 = HDLlize(b0)
    val ACK2 = HDLlize(b0)
    val ACK3 = HDLlize(b0)
    val ACK4 = HDLlize(b0)


    sync((RESET,0),(CLOCK,1)){
      when (RESET isnot 1){
        SD_COUNTER := 0x3f
      }.otherwise{
        when(GO is 0){
          SD_COUNTER := 0
        }.otherwise{
          when (SD_COUNTER < 41){
            SD_COUNTER := SD_COUNTER + 1
          }
        }
      }
    }

    sync((RESET,0),(CLOCK,1)){
      when (RESET isnot 1){
        SCLK := 1
        SDO := 1
        ACK1 := 0
        ACK2 := 0
        ACK3:= 0
        ACK4:= 0
        END := 1
      }.otherwise{
        when (SD_COUNTER is 0){
          ACK1 := 0
          ACK2 := 0
          ACK3 := 0
          ACK4 := 0
          END := 0
          SDO := 1
          SCLK := 1
        }
        when (SD_COUNTER is 1){
          SD := I2C_DATA
          SDO := 0
        }
        when (SD_COUNTER is 2){
          SCLK := 0
        }
        when (SD_COUNTER is 3){
          SDO := SD(31)
        }
        when (SD_COUNTER is 4){
          SDO := SD(30)
        }
        when (SD_COUNTER is 5){
          SDO := SD(29)
        }
        when (SD_COUNTER is 6){
          SDO := SD(28)
        }
        when (SD_COUNTER is 7){
          SDO := SD(27)
        }
        when (SD_COUNTER is 8){
          SDO := SD(26)
        }
        when (SD_COUNTER is 9){
          SDO := SD(25)
        }
        when (SD_COUNTER is 10){
          SDO := SD(24)
        }
        when (SD_COUNTER is 11){
          SDO := 1
        }
        when (SD_COUNTER is 12){
          SDO := SD(23)
          ACK1 := I2C_SDAT
        }
        when (SD_COUNTER is 13){
          SDO := SD(22)
        }
        when (SD_COUNTER is 14){
          SDO := SD(21)
        }
        when (SD_COUNTER is 15){
          SDO := SD(20)
        }
        when (SD_COUNTER is 16){
          SDO := SD(19)
        }
        when (SD_COUNTER is 17){
          SDO := SD(18)
        }
        when (SD_COUNTER is 18){
          SDO := SD(17)
        }
        when (SD_COUNTER is 19){
          SDO := SD(16)
        }
        when (SD_COUNTER is 20){
          SDO := 1
        }
        when (SD_COUNTER is 21){
          SDO := SD(15)
          ACK2 := I2C_SDAT
        }
        when (SD_COUNTER is 22){
          SDO := SD(14)
        }
        when (SD_COUNTER is 23){
          SDO := SD(13)
        }
        when (SD_COUNTER is 24){
          SDO := SD(12)
        }
        when (SD_COUNTER is 25){
          SDO := SD(11)
        }
        when (SD_COUNTER is 26){
          SDO := SD(10)
        }
        when (SD_COUNTER is 27){
          SDO := SD(9)
        }
        when (SD_COUNTER is 28){
          SDO := SD(8)
        }
        when (SD_COUNTER is 29){
          SDO := 1

        }
        when (SD_COUNTER is 30){
          SDO := SD(7)
          ACK3 := I2C_SDAT
        }
        when (SD_COUNTER is 31){
          SDO := SD(6)
        }
        when (SD_COUNTER is 32){
          SDO := SD(5)
        }
        when (SD_COUNTER is 33){
          SDO := SD(4)
        }
        when (SD_COUNTER is 34){
          SDO := SD(3)
        }
        when (SD_COUNTER is 35){
          SDO := SD(2)
        }
        when (SD_COUNTER is 36){
          SDO := SD(1)
        }
        when (SD_COUNTER is 37){
          SDO := SD(0)
        }
        when (SD_COUNTER is 38){
          SDO := 1
        }
        when (SD_COUNTER is 39){
          SDO := 0
          SCLK := 0
          ACK4 := I2C_SDAT
        }
        when (SD_COUNTER is 40){
          SCLK := 1
        }
        when (SD_COUNTER is 41){
          SDO := 1
          END := 1
        }


      }

    }
  }

  override val toCompile = List(I2CController)

}

object Main{
  def main(args: Array[String]): Unit ={
    new I2CController(b0,b0,b0,Unsigned(0,32),b0,b0,b0,b0).compile.toConsole
  }
}