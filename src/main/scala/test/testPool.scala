import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

class tp extends Component {
  val inp = Reg(FM(3,10,8,8,64,0,"weight\\resnet20.bin",resnet20().x.weightList))
  inp.fm.valid := True
  val p = AvgPool(inp, 8, stride = 1, padding = 0, Qop = 10, Qor = 10)
  val a = FindMax(p.fm)
}

object poolSim {
  def main(args : Array[String]) {
    SimConfig.withWave.doSim(new tp){dut =>//
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
      for(idx <- 0 until 3800) {

        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}
