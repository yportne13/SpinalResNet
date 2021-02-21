import spinal.core._
import spinal.lib._

object ReLu {
  def apply(inp : FM): FM = {
    val ret = FM(inp)
    ret.setWeakName("ReLuOut")
    val relu = new ReLu(inp.Qp, inp.Qr, inp.W)
    relu.io.inp := inp.fm
    ret.fm := relu.io.oup
    ret
  }
}

class ReLu(
  Qp : Int,
  Qr : Int,
  W : Int
) extends Component {
  val io = new Bundle {
    val inp = in (Flow(Vec(SFix(Qp exp, -Qr exp),W)))
    val oup = out (Flow(Vec(SFix(Qp exp, -Qr exp),W)))
  }

  val oup = RegFlow(Vec(SFix(Qp exp, -Qr exp),W))
  for(i <- 0 until W) {
    when(io.inp.payload(i) >= 0) {//inp > 0
      oup.payload(i) := io.inp.payload(i)
    }.otherwise {
      oup.payload(i) := 0
    }
  }
  oup.valid := io.inp.valid

  io.oup := oup
}
