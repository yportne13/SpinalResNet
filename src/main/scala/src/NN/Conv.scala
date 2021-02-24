import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

object conv2d {
  def apply(inp : FM, input_channel : Int, output_channel : Int, kernel_size : Int, stride : Int = 1, padding : Int = 0, bias: Boolean = false, Qop : Int, Qor : Int, ChoutDivHard : Int): FM = {
    println("Layer"+inp.Layer+":conv2d("+input_channel+","+output_channel+","+kernel_size+","+stride+","+padding+")"+" ("+((inp.W+2*padding-kernel_size)/stride+1)+","+((inp.H+2*padding-kernel_size)/stride+1)+")")
    val Win = inp.W
    val Hin = inp.H
    val Wout = (Win+2*padding-kernel_size)/stride+1
    val Hout = (Hin+2*padding-kernel_size)/stride+1
    val l = new Conv(input_channel,output_channel,kernel_size,stride,padding,inp.WeightFile,inp.WeightConfig,Win = inp.W, Hin = inp.H, Qip = inp.Qp, Qir = inp.Qr, Qop = Qop, Qor = Qor, layer = inp.Layer, SubNum = 0, DivNum = 0, ChoutDivHard = ChoutDivHard, noReLu = true, 0, "conv")
    l.io.input := inp.fm
    val oup = FM(Qop,Qor,Wout,Hout,output_channel,inp.Layer+1,inp.WeightFile,inp.WeightConfig)
    oup.fm := l.io.output
    oup
  }
}

object adder2d {
  def apply(inp : FM, input_channel : Int, output_channel : Int, kernel_size : Int, stride : Int = 1, padding : Int = 0, bias: Boolean = false, Qop : Int, Qor : Int, ChoutDivHard : Int): FM = {
    println("Layer"+inp.Layer+":adder2d("+input_channel+","+output_channel+","+kernel_size+","+stride+","+padding+")"+" ("+((inp.W+2*padding-kernel_size)/stride+1)+","+((inp.H+2*padding-kernel_size)/stride+1)+")")
    val Win = inp.W
    val Hin = inp.H
    val Wout = (Win+2*padding-kernel_size)/stride+1
    val Hout = (Hin+2*padding-kernel_size)/stride+1
    val l = new Conv(input_channel,output_channel,kernel_size,stride,padding,inp.WeightFile,inp.WeightConfig,Win = inp.W, Hin = inp.H, Qip = inp.Qp, Qir = inp.Qr, Qop = Qop, Qor = Qor, layer = inp.Layer, SubNum = 0, DivNum = 0, ChoutDivHard = ChoutDivHard, noReLu = true)
    l.io.input := inp.fm
    val oup = FM(Qop,Qor,Wout,Hout,output_channel,inp.Layer+1,inp.WeightFile,inp.WeightConfig)
    oup.fm := l.io.output
    oup
  }
}

class Conv(
  Chin  : Int,
  Chout : Int,
  kernel_size : Int,
  stride : Int,
  padding : Int,
  WeightFile : String,
  WeightConfig : List[List[Int]],
  Win        : Int,
  Hin        : Int,
  Qip        : Int,
  Qir        : Int,
  Qop        : Int,
  Qor        : Int,
  layer      : Int,
  SubNum     : Int,
  DivNum     : Int,
  ChoutDivHard : Int = 1,
  noReLu     : Boolean = false,
  highFreq   : Int = 1,
  mode       : String = "adder"
) extends Component {

  val Wout = (Win + 2 * padding - kernel_size) / stride + 1
  val Hout = (Hin + 2 * padding - kernel_size) / stride + 1

  val io = new Bundle {
    val input = in (Flow(Vec(SFix(Qip exp, -Qir exp),Win)))
    val output = out (Flow(Vec(SFix(Qop exp, -Qor exp),Wout)))
  } simPublic()

  val lcore = new ConvCore(Chin,Chout,kernel_size,ChoutDivHard,stride,padding,WeightFile,WeightConfig,Win,Hin,Qip,Qir,Qop,Qor,layer,highFreq,mode)
  lcore.io.valid_in := io.input.valid
  lcore.io.data_in  := io.input.payload

  val lcOut = Flow(Vec(SFix(Qop exp, -Qor exp),Wout))
  val lOut = Vec(Vec(Reg(SFix(Qop exp, -Qor exp)) init(0),Wout),Chout / ChoutDivHard)
  when(lcore.io.valid_out) {
    lOut := lcore.io.data_out
  }.otherwise {
    for(i <- 0 until Chout / ChoutDivHard - 1) {
      lOut(i) := lOut(i+1)
    }
  }
  
  val lcOutValid = Reg(Bool) init(False)
  when(lcore.io.valid_out) {
    lcOutValid := True
  }.elsewhen(Delay(lcore.io.valid_out,Chout / ChoutDivHard)) {
    lcOutValid := False
  }
  lcOut.valid   := lcOutValid
  lcOut.payload := (if(mode != "avgpool") lOut(0) else (Vec(lOut(0).map{x => val ret = cloneOf(x);ret.raw := x.raw / (kernel_size*kernel_size);ret})) )

  io.output := lcOut

}
