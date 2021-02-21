import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import java.io._

object ResNetSim {
  //val writer = new PrintWriter(new File("HardSave\\c2.txt" ))
  def main(args : Array[String]) {
    val (mat,label) = LoadCifar10()
    var oCnt = 0
    var successCnt = 0
    var delay = 864 * 200
    SimConfig.withWave.doSim(new ResNet()){dut =>//
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
      for(idx <- 0 until 40000) {
        if(idx%delay > 2 && idx%delay <= 2+4*32*3) {
          dut.io.inp.valid #= true
          for(i <- 0 until 8) {
            dut.io.inp.payload(i) #= (mat(idx/delay)((idx%delay - 3)%12/4)((idx%delay - 3)/12)(i+8*((idx%delay - 3)%4))*128).toInt
          }
        }else {
          dut.io.inp.valid #= false
          for(i <- 0 until 8) {
            dut.io.inp.payload(i) #= 0
          }
        }

        dut.clockDomain.waitRisingEdge()

        //if(dut.conv1.fm.valid.toBoolean) {
        //  for(i <- 0 until 32) {
        //    //print(dut.plotr1(i).toInt.toDouble / 1024 + ",")
        //    writer.write(dut.plotr1(i).toInt.toDouble / 1024 + ",")
        //    //print(dut.inp.fm.payload(i) + ",")//.asBits.toInt.toDouble / 256
        //  }
        //  writer.write("\n")
        //  //println()
        //}
        if(dut.io.oup.valid.toBoolean == true) {
          //print(dut.io.output.payload.toLong + ",")
          //print(label(oCnt))
          //println()
          if(label(oCnt) == dut.io.oup.payload.toLong) {
            successCnt = successCnt + 1
          }
          oCnt = oCnt + 1
        }
        if(idx%(delay*100) == 100) {
          println(successCnt + ";" + oCnt)
        }
      }
    }
    println(successCnt + ";" + oCnt)
  }
}