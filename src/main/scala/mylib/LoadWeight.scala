import java.io.{File, FileInputStream}

object LoadWeight {
  def apply(FileName : String, config : List[List[Int]]):List[Array[Double]] = {
    val filew = new File(FileName)
    val inw = new FileInputStream(filew)
    val bytesw = new Array[Byte](filew.length.toInt)
    inw.read(bytesw)
    //get the bytes(i*4)
    inw.close()

    def b2i(i1 : Byte, i2 : Byte, i3 : Byte, i4 : Byte): Int = {
      val t1 : Int = i1
      val t2 : Int = i2
      val t3 : Int = i3
      val t4 : Int = i4
      val r1 : Int = if(t1<0) t1+256 else t1
      val r2 : Int = if(t2<0) t2+256 else t2
      val r3 : Int = if(t3<0) t3+256 else t3
      var ret : Int = t4*256*256*256+r3*256*256+r2*256+r1
      ret
    }

    var idx = 0
    val w = config.map{x => 
      if(x.length == 1) {
        (0 until x(0)).map{y => 
        val ret = b2i(bytesw(4*idx),bytesw(4*idx+1),bytesw(4*idx+2),bytesw(4*idx+3))
        idx = idx + 1 
        ret.toDouble/1024}.toArray
      }else {
        val div = 1024
        val w = (0 until x(0)).map{y => 
                val ret = b2i(bytesw(4*idx),bytesw(4*idx+1),bytesw(4*idx+2),bytesw(4*idx+3)).toDouble / div
                idx = idx + 1
                ret}.toArray
        val b = (0 until x(0)).map{y => 
                val ret = b2i(bytesw(4*idx),bytesw(4*idx+1),bytesw(4*idx+2),bytesw(4*idx+3)).toDouble / div
                idx = idx + 1
                ret}.toArray
        val mean = (0 until x(0)).map{y => 
                val ret = b2i(bytesw(4*idx),bytesw(4*idx+1),bytesw(4*idx+2),bytesw(4*idx+3)).toDouble / div
                idx = idx + 1
                ret}.toArray
        val varX = (0 until x(0)).map{y => 
                var ret = b2i(bytesw(4*idx),bytesw(4*idx+1),bytesw(4*idx+2),bytesw(4*idx+3)).toDouble / div
                idx = idx + 1
                if(ret == 0) {ret = 100}//to avoid weight or bias be something div zero
                ret}.toArray
        idx = idx + 1
        val weight = (0 until x(0)).map{y => 
                val ret = w(y) / scala.math.sqrt(varX(y))
                //val ret = ( w(y) / varX(y) * 1024*32).toInt
                ret}.toList
        val bias = (0 until x(0)).map{y => 
                val ret = (b(y) - w(y) * mean(y) / scala.math.sqrt(varX(y)))
                //val ret = ( b(y) - w(y) * mean(y) / varX(y) * 1024*32).toInt
                ret}.toList
        (weight ::: bias).toArray
      }
    }

    w
  }
}