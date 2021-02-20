import java.io.{File, FileInputStream}

object LoadCifar10 {
  def apply():(Array[Array[Array[Array[Double]]]],Array[Int]) = {
    val mat = Array.ofDim[Double](10000,3,32,32)//new DenseMatrix[Double](10000,28,28)
    val label = new Array[Int](10000)

    val file = new File("../data/test_batch.bin")
    val in = new FileInputStream(file)
    val bytes = new Array[Byte](file.length.toInt)
    in.read(bytes)
    for(k <- 0 until 10000) {
      label(k) = bytes(k*3073).toInt
    }
    val norm = Array(Array(0.4914, 0.4822, 0.4465), Array(0.2023, 0.1994, 0.2010))
    for(k <- 0 until 10000) {
      for(i <- 0 until 3) {
        for(j <- 0 until 32) {
          for(m <- 0 until 32) {
            val t = k*3073+i*32*32+j*32+m+1
            if(bytes(t)<0) {
              mat(k)(i)(j)(m) = 256 + bytes(t).toInt.toDouble
            }else {
              mat(k)(i)(j)(m) = bytes(t).toInt.toDouble
            }
            mat(k)(i)(j)(m) = (mat(k)(i)(j)(m).toDouble / 256.0 - norm(0)(i))/norm(1)(i)
          }
        }
      }
    }
    //val f2 = Figure()
    //f2.subplot(0) += image(mat)
    in.close()
    (mat,label)
  }
}