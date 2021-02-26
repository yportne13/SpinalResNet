import java.io.{File, FileInputStream, PrintWriter}
import Net._

case class resnet20() extends Net {
  var x = NetConfig()
  x = conv2d(x,3,16,3,1,1)
  x = BatchNorm(x,16)
  x = relu(x)
  //
  x = adder2d(x,16,16,3,1,1)
  x = BatchNorm(x,16)
  x = relu(x)
  x = adder2d(x,16,16,3,1,1)
  x = BatchNorm(x,16)
  x = relu(x)
  x = adder2d(x,16,16,3,1,1)
  x = BatchNorm(x,16)
  x = relu(x)
  x = adder2d(x,16,16,3,1,1)
  x = BatchNorm(x,16)
  x = relu(x)
  x = adder2d(x,16,16,3,1,1)
  x = BatchNorm(x,16)
  x = relu(x)
  x = adder2d(x,16,16,3,1,1)
  x = BatchNorm(x,16)
  x = relu(x)
  //
  x = adder2d(x,16,32,3,2,1)
  x = BatchNorm(x,32)
  x = relu(x)
  x = adder2d(x,32,32,3,1,1)
  x = BatchNorm(x,32)
    //
    x = adder2d(x,16,32,1,2,1)
    x = BatchNorm(x,32)
    //
  x = relu(x)
  x = adder2d(x,32,32,3,1,1)
  x = BatchNorm(x,32)
  x = relu(x)
  x = adder2d(x,32,32,3,1,1)
  x = BatchNorm(x,32)
  x = relu(x)
  x = adder2d(x,32,32,3,1,1)
  x = BatchNorm(x,32)
  x = relu(x)
  x = adder2d(x,32,32,3,1,1)
  x = BatchNorm(x,32)
  x = relu(x)
  //
  x = adder2d(x,32,64,3,2,1)
  x = BatchNorm(x,64)
  x = relu(x)
  x = adder2d(x,64,64,3,1,1)
  x = BatchNorm(x,64)
    //
    x = adder2d(x,32,64,1,2,1)
    x = BatchNorm(x,64)
    //
  x = relu(x)
  x = adder2d(x,64,64,3,1,1)
  x = BatchNorm(x,64)
  x = relu(x)
  x = adder2d(x,64,64,3,1,1)
  x = BatchNorm(x,64)
  x = relu(x)
  x = adder2d(x,64,64,3,1,1)
  x = BatchNorm(x,64)
  x = relu(x)
  x = adder2d(x,64,64,3,1,1)
  x = BatchNorm(x,64)
  x = relu(x)
  //
  x = avgpool(x,8,1)
  x = conv2d(x,64,10,1)
  x = BatchNorm(x,10)
}

object GoldenResNet {
  def saveFM(inp : Array[Array[Array[Double]]], fileName : String) = {
    val writer = new PrintWriter(new File("saveFM\\"+fileName+".txt" ))
    for(j <- 0 until inp(0).length) {
      for(i <- 0 until inp.length) {
        for(k <- 0 until inp(0)(0).length) {
          writer.write(inp(i)(j)(k)+",")
        }
        writer.write("\n")
      }
      writer.write("\n\n")
    }
    writer.close()
  }
  def printFM(inp : Array[Array[Double]]) = {
    for(i <- 0 until inp.length) {
      for(j <- 0 until inp(0).length) {
        print(inp(i)(j)+",")
      }
      println()
    }
  }
  def BasicBlock(inp : Array[Array[Array[Double]]], inplanes : Int, planes : Int, weight : List[Array[Double]], stride : Int = 1, downsample : Int = 0): Array[Array[Array[Double]]]= {
    var residual = inp
    val conv1 = Golden.adder2d(inp, inplanes, planes, 3, weight(0), stride = stride)
    val bn1 = Golden.BatchNorm(conv1,weight(1))
    val r1 = Golden.relu(bn1)
    val conv2 = Golden.adder2d(r1, planes, planes, 3, weight(2))
    val bn2 = Golden.BatchNorm(conv2,weight(3))
    if(downsample == 1) {
      val convr = Golden.adder2d(inp, inplanes, planes, 1, weight(4), stride = stride, padding = 0)
      residual = Golden.BatchNorm(convr,weight(5))
    }
    if(inp(0)(0)(0) == 0.02147562182031093) {
      print("bs1b2")
      saveFM(bn2,"bs1b2")
    }
    //printFM(bn1(0))
    val resAdd = bn2.zipWithIndex.map{case (value1,idx1) => value1.zipWithIndex.map{case (value2,idx2) => value2.zipWithIndex.map{case (value3,idx3) => value3 + residual(idx1)(idx2)(idx3)}}}
    val r2 = Golden.relu(resAdd)
    r2
  }
  def makeLayer(inp : Array[Array[Array[Double]]], planes : Int, weight : List[Array[Double]], stride : Int = 1): Array[Array[Array[Double]]] = {
    var downsample = 0
    if(stride != 1 || inp.length != planes) {
      downsample = 1
    }

    val bs1 = BasicBlock(inp, inp.length, planes, (0 until 4+downsample*2).map(x => weight(x).map(_.toDouble).toArray).toList, stride = stride, downsample = downsample)
    if(inp(0)(0)(0) == 0.02147562182031093) {
      saveFM(bs1,"bs1")
    }
    val bs2 = BasicBlock(bs1, planes, planes, (4+downsample*2 until 8+downsample*2).map(x => weight(x).map(_.toDouble).toArray).toList)
    val bs3 = BasicBlock(bs2, planes, planes, (8+downsample*2 until 12+downsample*2).map(x => weight(x).map(_.toDouble).toArray).toList)
    bs3
  }
  def main(args : Array[String]) {
    val (mat,label) = LoadCifar10()

    val wList = LoadWeight("weight\\resnet20.bin",resnet20().x.weightList)

    var suc = 0
    var div = 1024
    for(i <- 0 until 1) {
      var l1 = Golden.conv2d(mat(i),3,16,3,wList(0),1,1)
      var b1 = Golden.BatchNorm(l1,wList(1))
      var r1 = Golden.relu(b1)

      saveFM(r1,"r1")
      var res1 = makeLayer(r1,16,(2 until 14).map(x => wList(x)).toList)
      saveFM(res1,"res1")
      //printFM(res1(0))
      val res2 = makeLayer(res1,32,(14 until 28).map(x => wList(x)).toList,stride = 2)
      val res3 = makeLayer(res2,64,(28 until 42).map(x => wList(x)).toList,stride = 2)
      saveFM(res2,"res2")
      val pool = Golden.AvgPool(res3,8,1)
      val fc   = Golden.conv2d(pool,64,10,1,wList(42),stride = 1, padding = 0)
      val bn   = Golden.BatchNorm(fc,wList(43))
      saveFM(pool,"pool")
      saveFM(bn,"bn")

      //println("weight:"+wList.length)
      //println("res1:"+res1.length+";"+res1(0).length+";"+res1(0)(0).length)
      //println("res2:"+res2.length+";"+res2(0).length+";"+res2(0)(0).length)
      //println("res3:"+res3.length+";"+res3(0).length+";"+res3(0)(0).length)
      //println("avg:"+pool.length+";"+pool(0).length+";"+pool(0)(0).length)

      //printFM(res1(0))
      //wList(0).map(x => println(x.toDouble/1024))

      //println(l1.map(_.map(_.max).max).max)
      //println(b1.map(_.map(_.max).max).max)
      //println(l1.map(_.map(_.min).min).min)
      //println(b1.map(_.map(_.min).min).min)
      //for(k <- 0 until 6) {
      //  for(j <- 0 until 6) {//mat(0)(0)(k)(j)
      //    print((b1(12)(k)(j))*256 + ",")
      //    //print(l1(0)(k)(j) + ",")
      //  }
      //  println()
      //}

      var max = bn(0)(0)(0)
      var index = 0
      for(j <- 0 until 10) {
        if(max < bn(j)(0)(0)) {
          max = bn(j)(0)(0)
          index = j
        }
      }
      //for(j <- 0 until 10) {
      //  print(l3(j)+",")
      //}
      //println()
      //println(index + "," + label(i))
      if(index == label(i)) {
        suc = suc + 1
      }
    }
    println(suc)

  }
}