import spinal.core._
import spinal.lib._

object FindMax {
  def apply(inp : Flow[Vec[SFix]]): Flow[UInt] = {
    val Q = inp.payload(0).asBits.getWidth
    val ret = Reg(UInt(log2Up(10) bits)) init(0)
    ret.setWeakName("ret")
    val cnt = Reg(UInt(log2Up(10) bits)) init(0)
    cnt.setWeakName("cnt")
    val max = Reg(cloneOf(inp.payload(0)))
    max.raw.init(-(1 << (Q-1)))
    max.setName("max_channel")
    when(inp.valid) {
      cnt := cnt + 1
    }.otherwise {
      cnt := 0
    }
    when(inp.valid) {
      when(inp.payload(0) > max) {
        max.raw := inp.payload(0).raw
        ret := cnt
      }
    }.otherwise {
      ret := 0
      max.raw := -(1 << (Q - 1))
    }
    //val valid = Reg(Bool) init(False)
    //valid.setWeakName("valid")
    //valid := 
    val oup = Flow(UInt(log2Up(10) bits))
    oup.setWeakName("findMaxOut")
    oup.payload := ret
    oup.valid   := inp.valid === False && Delay(inp.valid,1,init = False)
    oup
  }
}
