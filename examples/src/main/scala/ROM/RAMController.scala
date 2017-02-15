package NewHDLExample.RAMController

//import javax.comm._
import java.io._
import java.util._
/**
  * Created by user on 2016/11/21.
  */
/*class RAMController (s: Int, run: Int, clk: Int, we: Int, din: Int) {
  private val port = CommPortIdentifier.getPortIdentifier("COM3")
  private val serialPort :SerialPort = port.open("ScalaHDL", 200000).asInstanceOf[SerialPort]
  private val inputStream = serialPort.getInputStream
  private val outputStream = serialPort.getOutputStream

  def start : String ={
    /*val ans = ""
    val readBuffer = Array[Byte](20)
    serialPort.setSerialPortParams(115200,SerialPort.DATABITS_8,SerialPort.STOPBITS_1, SerialPort.PARITY_NONE)

    outputStream.write("\n".getBytes)
    //while (inputStream.available() <= 0)
    print(readL)
    outputStream.write("\n".getBytes)
    print(readL)
    ans*/
    "33"
  }
  def printSignal(name: String) :Int = {
    1
  }
  def setSignal(name: String) :Int = {
    1
  }
  def nextSteps(n: Int) :Int = {
    1
  }
  def readL : String = {
    val readBuffer = Array[Byte](20)
    var line = ""
    while (!readBuffer.contains(35)) {
      val i = inputStream.read(readBuffer)
      line = line.concat(new String(readBuffer))

    }
    line
  }

}
*/

