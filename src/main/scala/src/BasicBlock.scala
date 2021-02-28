import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._

object BasicBlock {
  def addFM(inp1 : FM, inp2 : FM, fifoDepth : Int = 1024): FM = {
    val fifoOut = Stream(cloneOf(inp2.fm.payload))
    fifoOut.ready := inp1.fm.valid
    fifoOut << inp2.fm.toStream.queue(fifoDepth)
    val ret = Reg(FM(inp1))
    ret.fm.valid.init(False)
    ret.fm.valid := inp1.fm.valid
    ret.fm.payload := Vec(inp1.fm.payload.zip(fifoOut.payload).map{case (value1,value2) => val ret = (value1 + value2).truncated;ret}.toList)
    ret
  }
  def apply(inp : FM, inplanes : Int, planes : Int, stride : Int = 1, downsample : Int = 0, fifoDepth : Int = 1024, ChoutDivHard : Int = 1): FM = {
    println("--BasicBlock("+inplanes+","+planes+")[")
    val Qr = 10
    val conv1 = adder2d(inp, inplanes, planes, 3, stride = stride, padding = 1, group = 1, bias = false, 12, Qr, ChoutDivHard = if((inplanes != planes) && 2*ChoutDivHard < inplanes) 2*ChoutDivHard else ChoutDivHard)
    val bn1 = BatchNorm(conv1,2,5)
    val r1 = ReLu(bn1)
    val conv2 = adder2d(r1, planes, planes, 3, 1, 1, group = 1, bias = false, 12, Qr, ChoutDivHard)
    val bn2 = BatchNorm(conv2,2,5)
    if(downsample == 1) {
      val convr = adder2d(inp.setLayer(bn2.Layer), inplanes, planes, 1, stride = stride, padding = 0, group = 1, bias = false, 12, Qr, if(inplanes != planes) 2*ChoutDivHard else ChoutDivHard)
      val residual = BatchNorm(convr,2,5)
      val resAdd = addFM(bn2,residual,fifoDepth)
      val r2 = ReLu(resAdd)
      println("--]")
      r2.setLayer(residual.Layer)
    } else {
      val residual = inp
      val resAdd = addFM(bn2,residual,fifoDepth)
      val r2 = ReLu(resAdd)
      println("--]")
      r2
    }
  }
}

object makeLayer {
  def apply(inp : FM, planes : Int, stride : Int = 1, fifoDepth : Int = 1024, fifoDepth2 : Int = 1024, ChoutDivHard : Int = 1): FM = {
    var downsample = 0
    if(stride != 1 || inp.Channel != planes) {
      downsample = 1
    }
    println("-MakeLayer("+inp.Channel+","+planes+","+downsample+"){")
    val bs1 = BasicBlock(inp, inp.Channel, planes, stride = stride, downsample = downsample, fifoDepth = fifoDepth, ChoutDivHard = ChoutDivHard)
    val bs2 = BasicBlock(bs1, planes, planes, fifoDepth = fifoDepth2, ChoutDivHard = ChoutDivHard)
    val bs3 = BasicBlock(bs2, planes, planes, fifoDepth = fifoDepth2, ChoutDivHard = ChoutDivHard)
    println("-}")
    bs3
  }
}
