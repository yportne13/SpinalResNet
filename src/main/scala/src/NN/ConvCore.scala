import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

class ConvCore(
  Chin  : Int,
  Chout : Int,
  kernel_size : Int,
  ChoutDivHard : Int,
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
  highFreq   : Int = 1,
  mode       : String = "adder"
) extends Component {

  val Wout = (Win + 2 * padding - kernel_size) / stride + 1
  val Hout = (Hin + 2 * padding - kernel_size) / stride + 1
  val io = new Bundle {
    val valid_in = in Bool
    val data_in  = in Vec(SFix(Qip exp, -Qir exp),Win)
    val valid_out = out Bool
    val data_out = out Vec(Vec(SFix(Qop exp, -Qor exp),Wout),Chout / ChoutDivHard)//Vec(UInt(Q bits),Wout)
  } simPublic()

  //layer ram
  val FMram = Mem(Vec(SFix(Qip exp, -Qir exp),Win),wordCount = (Hin+2*padding)*Chin*2)
  FMram.init(Vec((0 until (Hin+2*padding)*Chin*2).map(x => Vec((0 until Win).map{x => val ret = SFix(Qip exp, -Qir exp);ret := 0;ret}.toList))))//init the ram with all 0
  val pingpongW = Reg(Bool) init(False)
  val pingpongR = Reg(Bool) init(True)
  val faddw = Reg(UInt(log2Up((Hin+2*padding)*Chin) bits)) init(padding * Chin)
  when(io.valid_in) {
    when(faddw < (Hin+padding)*Chin - 1) {
      faddw := faddw + 1
    }.otherwise {
      faddw := padding * Chin
    }
  }
  FMram.write(
    address = U(faddw ## pingpongW),
    data    = io.data_in,
    enable  = io.valid_in
  )

  val start = Reg(Bool) init(False)
  when(faddw === (Hin+padding)*Chin - 1 && io.valid_in) {
    start := True
    pingpongR := !pingpongR
    pingpongW := !pingpongW
  }.otherwise {
    start := False
  }

  //ctrl
  val ctrl = new ConvCtrl(Chin = Chin, kernel_size, ChoutDivHard = ChoutDivHard, high = Hout, Hin = Hin, stride = stride, padding = padding)
  ctrl.io.start := start

  //feature map
  val FMramOut = Vec(SFix(Qip exp, -Qir exp),Win)
  FMramOut := FMram.readSync(U(ctrl.io.faddr ## pingpongR))
  val peFM = Vec(Reg(SFix(Qip exp, -Qir exp)) init(0),Win + 2*padding)
  when(Delay(ctrl.io.shift,1)) {
    for(i <- 0 until Win + 2 * padding - 1) {
      peFM(i) := peFM(i + 1)
    }
  }.otherwise {
    if(padding == 1) {
      peFM(0) := 0
      peFM(Win + padding) := 0
      for(i <- 0 until Win) {
        peFM(i+1) := FMramOut(i)
      }
    }else {
      for(i <- 0 until Win) {
        peFM(i) := FMramOut(i)
      }
    }
  }

  //weight
  val wList = LoadWeight(WeightFile,WeightConfig)
  val w = wList(layer)
  val Qwp : Int = log2Up(w.map(x => scala.math.abs(x)).max.toInt+1)
  val Qwr : Int = Array(log2Up((1/w.map(x => scala.math.abs(x)).max).toInt) + 10,10).max
  val wrom = new Wrom(w, Chout = Chout, ChoutDivHard = ChoutDivHard)
  wrom.io.addr := Delay(ctrl.io.waddr,1)

  val pe = new PE(Wout = Wout, Chout = Chout / ChoutDivHard, Qwp = Qwp, Qwr = Qwr, Qfmp = Qip, Qfmr = Qir, Qop = Qop, Qor = Qor, highFreq = highFreq, mode = mode)
  pe.io.clear := ctrl.io.clear
  for(i <- 0 until Wout) {
    pe.io.FM(i) := peFM(i * stride)
  }
  //or to write as:  pe.io.FM := Vec()
  pe.io.W  := wrom.io.w

  io.valid_out := Delay(ctrl.io.valid,4+highFreq,init = False)
  io.data_out := pe.io.oup//peOut(0)

}
