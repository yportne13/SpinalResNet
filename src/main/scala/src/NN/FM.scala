import spinal.core._
import spinal.lib._

object FM {
  def apply(inp : FM, LayerAdd : Int = 0): FM = {
    val ret = FM(inp.Qp, inp.Qr, inp.W, inp.H, inp.Channel, inp.Layer+LayerAdd, inp.WeightFile, inp.WeightConfig)
    ret
  }
}

case class FM(
  Qp : Int,
  Qr : Int,
  W : Int,
  H : Int,
  Channel : Int,
  Layer : Int,
  WeightFile : String,
  WeightConfig : List[List[Int]]
) extends Bundle {

  val fm = Flow(Vec(SFix(Qp exp, -Qr exp),W))

  def setLayer(layer : Int): FM = {
    val ret = FM(Qp,Qr,W,H,Channel,layer,WeightFile,WeightConfig)
    ret.fm := fm
    ret
  }

}
