import spinal.core._
import spinal.lib._

class PE(
  Chout : Int = 8,
  Wout : Int = 9,
  Qwp  : Int = 5,
  Qwr  : Int = 7,
  Qfmp : Int = 5,
  Qfmr : Int = 7,
  Qop  : Int = 5,
  Qor  : Int = 7,
  highFreq : Int = 1,
  mode : String = "adder"
) extends Component {

  def abs(inp : SFix): SFix = {
    val ret = cloneOf(inp)//TODO
    when(inp >= 0) {
      ret.raw := -inp.raw
    }.otherwise {
      ret.raw := inp.raw
    }
    ret
  }

  val io = new Bundle {
    val clear = in Bool
    val W  = in Vec(SFix(Qwp  exp, -Qwr  exp),Chout)
    val FM = in Vec(SFix(Qfmp exp, -Qfmr exp),Wout)
    val oup = out Vec(Vec(SFix(Qop exp, -Qor exp),Wout),Chout)
  }

  val oup = Vec(Vec(Reg(SFix(Qop exp, -Qor exp)) init(0),Wout),Chout)
  val absOut = Vec(Vec(Reg(SFix((Array(Qfmp,Qwp).max+1) exp, -Array(Qfmr,Qwr).max exp)) init(0),Wout),Chout)
  val convOut = Vec(Vec(Reg(SFix((Qfmp+Qwp) exp, -Qor exp)) init(0),Wout),Chout)
  val avgOut = Vec(Vec(Reg(SFix(Qfmp exp, -Qfmr exp)) init(0),Wout),Chout)
  for(i <- 0 until Chout) {
    for(j <- 0 until Wout) {
      convOut(i)(j) := (io.FM(j) * io.W(i)).truncated//Delay((io.FM(j) * io.W(i)),highFreq)
      absOut(i)(j) := abs(Delay((io.FM(j) - io.W(i)),highFreq))//(io.FM(j) -^ io.W(i)).abs.resize(Array(Qfm,Qw).max bits)
      avgOut(i)(j) := io.FM(j)
    }
  }
  
  when(Delay(io.clear,3+highFreq)) {
    for(i <- 0 until Chout) {
      for(j <- 0 until Wout) {
        if(mode == "conv") {
          oup(i)(j) := convOut(i)(j).truncated
        }else if(mode == "adder"){
          oup(i)(j) := absOut(i)(j).truncated
        }else if(mode == "avgpool") {
          oup(i)(j) := avgOut(i)(j).truncated
        }
      }
    }
  }.otherwise {
    for(i <- 0 until Chout) {
      for(j <- 0 until Wout) {
        if(mode == "conv") {
          oup(i)(j) := (oup(i)(j) + convOut(i)(j)).truncated
        }else if(mode == "adder"){
          oup(i)(j) := (oup(i)(j) + absOut(i)(j)).truncated
        }else if(mode == "avgpool") {
          oup(i)(j) := (oup(i)(j) + avgOut(i)(j)).truncated
        }
      }
    }
  }
  io.oup := oup

}
