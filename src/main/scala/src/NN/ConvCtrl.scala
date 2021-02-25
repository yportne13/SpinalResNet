import spinal.core._
import spinal.lib._

class ConvCtrl(
  Chin    : Int,
  kernel_size : Int,
  ChoutDivHard : Int,
  high    : Int,
  Hin     : Int,
  stride  : Int,
  padding : Int,
  group   : Int = 1
) extends Component {
  val io = new Bundle {
    val start = in Bool
    val faddr = out UInt(log2Up((Hin+2*padding) * Chin) bits)
    val waddr = out UInt(log2Up(Chin * kernel_size * kernel_size * ChoutDivHard) bits)
    val clear = out Bool
    val shift = out Bool
    val valid = out Bool
  }

  //val cnt1 = Reg(UInt(2 bits)) init(0)//shift
  //val cntChin = Reg(UInt(log2Up(Chin) bits)) init(0)
  //val cnt2 = Reg(UInt(2 bits)) init(0)
  //val cntC = Reg(UInt(log2Up(ChoutDivHard) bits)) init(0)
  //val cntH = Reg(UInt(log2Up(high) bits)) init(0)
  val List(cnt1,cntChin,cnt2,cntC,cntH,cntGroup) = MultiCnt(io.start,List(kernel_size,Chin/group,kernel_size,ChoutDivHard,high,group))

  val faddr = Reg(UInt(log2Up((Hin + 2*padding) * Chin) bits)) init(padding*Chin)
  val cntGroupMult = if(group == 1) U(0,0 bits) else cntGroup * (Chin/group)
  if(high > 1) {
    faddr := cntChin + cntGroupMult + (cnt2 * Chin)(Array((cnt2*Chin).getWidth,log2Up((Hin + 2*padding) * Chin)).min - 1 downto 0) + (cntH * Chin * stride)(log2Up((Hin + 2*padding) * Chin) - 1 downto 0)
  }else {
    faddr := cntChin + cntGroupMult + (cnt2 * Chin)(log2Up(Hin * Chin) - 1 downto 0)
  }//TODO

  val waddr = Reg(UInt(log2Up(Chin * kernel_size * kernel_size * ChoutDivHard) bits)) init(0)
  if(ChoutDivHard > 1) {
    waddr := (cntC * kernel_size * kernel_size * Chin)(log2Up(Chin * kernel_size * kernel_size * ChoutDivHard) - 1 downto 0) + (cntChin + cntGroupMult) * kernel_size * kernel_size + cnt2 * kernel_size + cnt1
  }else {
    waddr := ((cntChin + cntGroupMult) * kernel_size * kernel_size)(log2Up(Chin*kernel_size*kernel_size*ChoutDivHard) - 1 downto 0) + cnt2 * kernel_size + cnt1
  }

  val clear = Reg(Bool) init(False)
  when(cnt1 === 0 && cntChin === 0 && cnt2 === 0) {
    clear := True
  }.otherwise {
    clear := False
  }

  val shift = Reg(Bool) init(False)
  when(cnt1 > 0) {
    shift := True
  }.otherwise {
    shift := False
  }

  val valid = Reg(Bool) init(False)
  valid := cnt1 === kernel_size - 1 && cnt2 === kernel_size - 1 && cntChin === Chin / group - 1

  io.faddr := faddr
  io.waddr := waddr
  io.clear := clear
  io.shift := shift
  io.valid := valid
}
