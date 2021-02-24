import spinal.core._
import spinal.lib._

object AvgPool {
  def apply(inp : FM, kernel_size : Int, stride : Int = 1, padding : Int = 0, bias: Boolean = false, Qop : Int, Qor : Int, ChoutDivHard : Int): FM = {
    println("Layer"+inp.Layer+":AvgPool("+kernel_size+","+stride+","+padding+")"+" ("+((inp.W+2*padding-kernel_size)/stride+1)+","+((inp.H+2*padding-kernel_size)/stride+1)+")")
    val Win = inp.W
    val Hin = inp.H
    val Wout = (Win+2*padding-kernel_size)/stride+1
    val Hout = (Hin+2*padding-kernel_size)/stride+1
    val l = new Conv(inp.Channel,inp.Channel,kernel_size,stride,padding,inp.WeightFile,inp.WeightConfig,Win = inp.W, Hin = inp.H, Qip = inp.Qp, Qir = inp.Qr, Qop = Qop, Qor = Qor, layer = inp.Layer, SubNum = 0, DivNum = 0, ChoutDivHard = ChoutDivHard, noReLu = true, 0, "avgpool")
    l.io.input := inp.fm
    val oup = FM(Qop,Qor,Wout,Hout,inp.Channel,inp.Layer+1,inp.WeightFile,inp.WeightConfig)
    oup.fm := l.io.output
    oup
  }
}
