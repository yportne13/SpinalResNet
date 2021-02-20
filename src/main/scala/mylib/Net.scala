package Net

case class NetConfig() {
  var weightList = List[List[Int]]()
}

trait Net {
  def conv2d(x : NetConfig, input_channel : Int, output_channel : Int, kernel_size : Int, stride : Int = 1, padding : Int = 0, bias: Boolean = false): NetConfig = {
    x.weightList = x.weightList :+ List((input_channel * output_channel * kernel_size * kernel_size))
    x
  }
  def adder2d(x : NetConfig, input_channel : Int, output_channel : Int, kernel_size : Int, stride : Int = 1, padding : Int = 0, bias: Boolean = false): NetConfig = {
    x.weightList = x.weightList :+ List((input_channel * output_channel * kernel_size * kernel_size))
    x
  }
  def BatchNorm(x : NetConfig, input_channel : Int): NetConfig = {
    x.weightList = x.weightList :+ List(input_channel,input_channel,input_channel,input_channel,1)
    x
  }
  def relu(x : NetConfig): NetConfig = {
    x
  }
  def avgpool(x : NetConfig, kernel_size : Int, stride : Int = 1): NetConfig = {
    x
  }
}

object Golden {
  def conv2d(inp : Array[Array[Array[Double]]], ic : Int, oc : Int, kernel_size : Int, w : Array[Double], stride : Int = 1, padding : Int = 1): Array[Array[Array[Double]]] = {
    val wide = inp(0).length + padding*2
    val wide2 = inp(0)(0).length + padding*2
    var tinp = Array.ofDim[Double](ic,wide,wide2)
    if(padding == 1) {
      for(i <- 0 until ic) {
        for(j <- 0 until wide) {
          for(k <- 0 until wide2) {
            if(j == 0 || j == wide-1 || k == 0 || k == wide2-1) {
              tinp(i)(j)(k) = 0
            }else {
              tinp(i)(j)(k) = inp(i)(j-1)(k-1)
            }
          }
        }
      }
    }else {
      tinp = inp
    }
    val wideout = (inp(0).length+2*padding-kernel_size)/stride+1
    val wideout2 = (inp(0)(0).length+2*padding-kernel_size)/stride+1
    val oup = Array.ofDim[Double](oc,wideout,wideout)
    for(x <- 0 until wideout) {//calculate conv
      for(y <- 0 until wideout2) {
        for(i <- 0 until oc) {
          for(j <- 0 until ic) {
            for(m <- 0 until kernel_size) {
              for(n <- 0 until kernel_size) {
                oup(i)(x)(y) = oup(i)(x)(y) + (tinp(j)(stride * x+m)(stride * y+n) * w(i*kernel_size*kernel_size*ic+j*kernel_size*kernel_size+m*kernel_size+n))
              }
            }
          }
        }
      }
    }
    oup
  }
  def adder2d(inp : Array[Array[Array[Double]]], ic : Int, oc : Int, kernel_size : Int, w : Array[Double], stride : Int = 1, padding : Int = 1): Array[Array[Array[Double]]] = {
    val wide = inp(0).length + padding*2
    val wide2 = inp(0)(0).length + padding*2
    var tinp = Array.ofDim[Double](ic,wide,wide2)
    if(padding == 1) {
      for(i <- 0 until ic) {
        for(j <- 0 until wide) {
          for(k <- 0 until wide2) {
            if(j == 0 || j == wide-1 || k == 0 || k == wide-1) {
              tinp(i)(j)(k) = 0
            }else {
              tinp(i)(j)(k) = inp(i)(j-1)(k-1)
            }
          }
        }
      }
    }else {
      tinp = inp
    }
    val wideout = (inp(0).length+2*padding-kernel_size)/stride+1
    val wideout2 = (inp(0)(0).length+2*padding-kernel_size)/stride+1
    val oup = Array.ofDim[Double](oc,wideout,wideout)
    for(x <- 0 until wideout) {//calculate conv
      for(y <- 0 until wideout2) {
        for(i <- 0 until oc) {
          for(j <- 0 until ic) {
            for(m <- 0 until kernel_size) {
              for(n <- 0 until kernel_size) {
                oup(i)(x)(y) = oup(i)(x)(y) - (tinp(j)(stride * x+m)(stride * y+n) - w(i*kernel_size*kernel_size*ic+j*kernel_size*kernel_size+m*kernel_size+n)).abs
              }
            }
          }
        }
      }
    }
    oup
  }
  def BatchNorm(inp : Array[Array[Array[Double]]], weight : Array[Double]): Array[Array[Array[Double]]] = {
    val oup = inp.zipWithIndex.map{case (value,idx) => value.map(_.map(x => x * weight(idx) + weight(inp.length + idx)).toArray).toArray}.toArray
    oup
  }
  def relu(inp : Array[Array[Array[Double]]]): Array[Array[Array[Double]]] = {
    inp.map(_.map(_.map(xi => if(xi>=0)xi else 0)))
  }
  def AvgPool(inp : Array[Array[Array[Double]]], kernel_size : Int, stride : Int, padding : Int = 0): Array[Array[Array[Double]]] = {
    val wide = inp(0).length + padding*2
    val wide2 = inp(0)(0).length + padding*2
    var tinp = Array.ofDim[Double](inp.length,wide,wide2)
    if(padding == 1) {
      for(i <- 0 until inp.length) {
        for(j <- 0 until wide) {
          for(k <- 0 until wide2) {
            if(j == 0 || j == wide-1 || k == 0 || k == wide2-1) {
              tinp(i)(j)(k) = 0
            }else {
              tinp(i)(j)(k) = inp(i)(j-1)(k-1)
            }
          }
        }
      }
    }else {
      tinp = inp
    }
    val wideout = (inp(0).length+2*padding-kernel_size)/stride+1
    val wideout2 = (inp(0)(0).length+2*padding-kernel_size)/stride+1
    val oup = Array.ofDim[Double](inp.length,wideout,wideout2)
    for(c <- 0 until inp.length) {
      for(x <- 0 until wideout) {
        for(y <- 0 until wideout2) {
          var Isum : Double = 0
          for(m <- 0 until kernel_size) {
            for(n <- 0 until kernel_size) {
              Isum = Isum + tinp(c)(x+m)(y+n)
            }
          }
          oup(c)(x)(y) = Isum / kernel_size / kernel_size
        }
      }
    }
    oup
  }
}