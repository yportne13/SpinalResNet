import spinal.core._
import spinal.lib._

object FindMax {
  def apply(inp : Flow[Vec[SInt]]): Flow[UInt] = {
    val Q = inp.payload(0).getWidth
    val ret = Reg(UInt(log2Up(10) bits)) init(0)
    ret.setWeakName("ret")
    val cnt = Reg(UInt(log2Up(10) bits)) init(0)
    cnt.setWeakName("cnt")
    val max = Reg(SInt(Q bits)) init((1 << (Q - 1))-1)
    max.setWeakName("max")
    when(inp.valid) {
      cnt := cnt + 1
    }.otherwise {
      cnt := 0
    }
    when(inp.valid) {
      when(inp.payload(0) < max) {
        max := inp.payload(0)
        ret := cnt
      }
    }.otherwise {
      ret := 0
      max := (1 << (Q - 1))-1
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
