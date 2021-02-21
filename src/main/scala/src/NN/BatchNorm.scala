import spinal.core._
import spinal.lib._

object BatchNorm {
  def apply(inp : FM): FM = {
    println("Layer"+inp.Layer+":BN("+inp.Channel+")")
    val weight = LoadWeight(inp.WeightFile,inp.WeightConfig)(inp.Layer)
    val w = (0 until inp.Channel).map(x => weight(x)).toList
    val b = (0 until inp.Channel).map(x => weight(x + inp.Channel)).toList
    val bn = new BatchNorm(inp.Qp, inp.Qr, inp.W, log2Up(w.max.toInt) + 1, 12, log2Up(b.max.toInt)+1, 12, inp.Channel, w, b)//TODO:Qr
    bn.io.inp := inp.fm
    val ret = FM(inp,1)
    ret.fm := bn.io.oup
    ret
  }
}

class BatchNorm(
  Qp : Int,
  Qr : Int,
  W : Int,
  Qw1p : Int,
  Qw1r : Int,
  Qw2p : Int,
  Qw2r : Int,
  Ch : Int,
  w : List[Double],
  b : List[Double]
) extends Component {
  val io = new Bundle {
    val inp = in (Flow(Vec(SFix(Qp exp, -Qr exp),W)))
    val oup = out (Flow(Vec(SFix(Qp exp, -Qr exp),W)))
  }

  val weight = Reg(Vec(SFix(Qw1p exp, -Qw1r exp),Ch))
  weight.zipWithIndex.map{case (value,idx) => value.init(w(idx))}
  val bias = Reg(Vec(SFix(Qw2p exp, -Qw2r exp),Ch))
  bias.zipWithIndex.map{case (value,idx) => value.init(b(idx))}
  when(io.inp.valid) {
    weight(Ch - 1) := weight(0)
    bias(Ch - 1) := bias(0)
    for(i <- 0 until Ch - 1) {
      weight(i) := weight(i+1)
      bias(i) := bias(i+1)
    }
  }
  val oup = Vec(Reg(SFix(Qp exp, -Qr exp)) init(0),W)
  for(i <- 0 until W) {
    oup(i) := (Delay((weight(0) * io.inp.payload(i)).setWeakName("Mult"),1) + Delay(bias(0),1)).truncated
  }
  io.oup.payload := oup
  io.oup.valid := Delay(io.inp.valid,2,init = False)

}
