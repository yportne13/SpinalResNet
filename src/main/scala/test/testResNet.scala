import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

import java.io._

object ResNetSim {
  val writer1 = new PrintWriter(new File("HardSave\\res1.txt" ))
  val writer2 = new PrintWriter(new File("HardSave\\res2.txt" ))
  val writer3 = new PrintWriter(new File("HardSave\\res3.txt" ))
  val writerp = new PrintWriter(new File("HardSave\\pool.txt" ))
  val writero = new PrintWriter(new File("HardSave\\out.txt" ))
  def main(args : Array[String]) {
    val (mat,label) = LoadCifar10()
    var oCnt = 0
    var successCnt = 0
    var delay = 20000//18432
    SimConfig.doSim(new ResNet()){dut =>//.withWave
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
      for(idx <- 0 until 50000000) {
        if(idx%delay > 2 && idx%delay <= 2+4*32*3) {
          dut.io.inp.valid #= true
          for(i <- 0 until 8) {
            dut.io.inp.payload(i) #= (mat(idx/delay)((idx%delay - 3)%12/4)((idx%delay - 3)/12)(i+8*((idx%delay - 3)%4))*1024).toInt
          }
        }else {
          dut.io.inp.valid #= false
          for(i <- 0 until 8) {
            dut.io.inp.payload(i) #= 0
          }
        }

        dut.clockDomain.waitRisingEdge()

        //if(dut.res1.fm.valid.toBoolean) {
        //  for(i <- 0 until 32) {
        //    writer1.write(dut.plot1(i).toInt.toDouble / 1024  /16 + ",")
        //  }
        //  writer1.write("\n")
        //}
        //if(dut.res2.fm.valid.toBoolean) {
        //  for(i <- 0 until 16) {
        //    writer2.write(dut.plot2(i).toInt.toDouble / 1024 / 16 + ",")
        //  }
        //  writer2.write("\n")
        //}
        //if(dut.res3.fm.valid.toBoolean) {
        //  for(i <- 0 until 8) {
        //    writer3.write(dut.plot3(i).toInt.toDouble / 1024 / 16 + ",")
        //  }
        //  writer3.write("\n")
        //}
        //if(dut.pool.fm.valid.toBoolean) {
        //  writerp.write(dut.plotp(0).toInt.toDouble / 1024 + ",")
        //  writerp.write("\n")
        //}
        //if(dut.bn.fm.valid.toBoolean) {
        //  for(i <- 0 until 1) {
        //    writero.write(dut.ploto(i).toInt.toDouble / 1024 / 16 + ",")
        //  }
        //  writero.write("\n")
        //}
        if(dut.io.oup.valid.toBoolean == true) {
          if(label(oCnt) == dut.io.oup.payload.toLong) {
            successCnt = successCnt + 1
          }
          oCnt = oCnt + 1
        }
        if(idx%(delay*5) == 0) {
          println(successCnt + ";" + oCnt)
        }
      }
    }
    println(successCnt + ";" + oCnt)
    writer1.close()
    writer2.close()
    writer3.close()
    writerp.close()
    writero.close()
  }
}