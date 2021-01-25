package com.reremouse.dml.operation

import breeze.linalg._
import com.reremouse.dml.model.{RereData, RereDataCenter}
import com.reremouse.util.RereLogger

/**
  * Created by RereMouse on 2018-01-21.
  */
object SVDTransformer {

  val logger = RereLogger.getLogger(this.getClass)

  /**
    *
    * @param dataSet
    * @return
    */
  def transform(dataSet: Seq[RereData]): Seq[RereData] = {
    val k=dataSet.length
    transform(dataSet,k)
  }

  /**
    *
    * @param dataSet
    * @param k
    * @return
    */
  def transform(dataSet: Seq[RereData], k: Int): Seq[RereData] = {
    transformAndReturnSV(dataSet,k)._1
  }

  /**
    *
    * @param dataSet
    * @param k
    * @return
    */
  def transformAndReturnSV(dataSet: Seq[RereData], k: Int): (Seq[RereData],DenseVector[Double]) = {
    val dim = dataSet(0).getData.length
    val len = dataSet.length
    val mat: DenseMatrix[Double] = DenseMatrix.zeros[Double](dim, len)
    for (i <- 0 until len) {
      mat(::, i) := dataSet(i).getData
    }
    println(mat)

//    val m1 = mat(20, ::)
//    val m2 = mat(31, ::)
//    println(m1 * m2.t)
    val svd.SVD(u, s, v) = svd(mat)
    //println(v.rows+":"+v.cols)
    val d = diag(s) * (v(0 until s.length, ::))
    val d1 = d(20, ::)
    val d2 = d(31, ::)
    println(d1 * d2.t)
    //UT为投影矩阵
    //val prj = u(::,0 until k).t
    for (i <- 0 until len) {
      val x = dataSet(i)
      x.setData(d(::, i))
    }
    (dataSet,s)
  }

  def main(args: Array[String]): Unit = {
    val input = "H:\\dml_experiments\\mnist.arff"
    val reader = new RereDataCenter
    val data = reader.loadWekaData(input)
    val newData = SVDTransformer.transform(data, 10)
    val d1 = newData(0).data
    val d2 = newData(1).data
    val d3 = newData(2).data
    //println(d1)
    //println(d2)
    println(d1.t * d2)
    println(d1.t * d3)
  }

}
