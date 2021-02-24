import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._

class ResNet extends Component {
  val io = new Bundle {
    val inp = in (Flow(Vec(SInt(13 bits),8)))
    val oup = out (Flow(UInt(4 bits)))
  }

  val l1in = Vec(Reg(SFix(3 exp, -9 exp)) init(0),32)
  when(io.inp.valid) {
    for(i <- 0 until 8) {
      l1in(i+24).raw := io.inp.payload(i)
    }
    for(i <- 0 until 24) {
      l1in(i) := l1in(i+8)
    }
  }
  val cnt = Reg(UInt(2 bits)) init(0)
  when(io.inp.valid) {
    when(cnt < 3) {
      cnt := cnt + 1
    }.otherwise {
      cnt := 0
    }
  }.otherwise {
    cnt := 0
  }

  val inp = FM(3,9,32,32,3,0,"weight\\resnet20.bin",resnet20().x.weightList) simPublic()
  inp.fm.valid := Delay(cnt === 3,1,init = False)
  inp.fm.payload := l1in

  val l1 = conv2d(inp,3,16,3,1,1,false,15,10,ChoutDivHard = 2) simPublic()
  val b1 = BatchNorm(l1)
  val r1 = ReLu(b1) simPublic()

  val res1 = makeLayer(r1, 16) simPublic()
  val res2 = makeLayer(res1,32,stride = 2) simPublic()
  val res3 = makeLayer(res2,64,stride = 2) simPublic()
  val plot = Vec(res3.fm.payload.map(x => x.asBits).toList) simPublic

  io.oup.valid := False
  io.oup.payload := 0
}