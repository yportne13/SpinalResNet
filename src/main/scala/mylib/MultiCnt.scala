import spinal.core._
import spinal.lib._

object MultiCnt {
  def BeforeEnd(cnt : List[UInt], inp : List[Int]): Bool = {
    val num = inp.zipWithIndex.map{case (value,idx) => if(value > 1) (value,idx) else null}.filter(_ != null)//num = (inp != 1)
    val ret = Reg(Bool)
    ret := num.zipWithIndex.map{case ((value1,value2),idx) => cnt(value2) === value1 - (if(idx == 0) 2 else 1)}.reduce(_ && _)
    ret
  }
  def apply(start : Bool, inp : List[Int]): List[UInt] = {
    val cnt = inp.map(x => Reg(UInt(log2Up(x) bits)) init(0))

    //val beforeEnd = Reg(Bool)
    //beforeEnd := inp.zipWithIndex.map{case (value,idx) =>
    //  if(idx == 0) {
    //    cnt(idx) === inp(idx) - 2
    //  }else {
    //    cnt(idx) === inp(idx) - 1
    //  }
    //}.reduce(_ && _)
    val beforeEnd = BeforeEnd(cnt,inp)
    val en = Reg(Bool) init(False)
    when(start) {
      en := True
    }.elsewhen(beforeEnd) {
      en := False
    }
    
    inp.zipWithIndex.map{case (value,idx) => 
      if(value > 1) {
        when((List(if(idx > 1) True else en,True) ::: ((0 until idx).map{case x => cnt(x) === inp(x) - 1}).toList).reduce(_ && _)) {//reduce need at least 2 param, so give two True(or 1 en 1 True) to avoid error, normally EDA could ignore this useless True
          when(cnt(idx) < value - 1) {
            cnt(idx) := cnt(idx) + 1
          }.otherwise {
            cnt(idx) := 0
          }
        }
      }
    }
    cnt
  }
}
