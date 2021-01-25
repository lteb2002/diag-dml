package com.reremouse.dml.model

import breeze.linalg._

import scala.beans.BeanProperty

/**
  * Created by RereMouse on 2018-01-17.
  */
class DistanceMetricsSum(m: Int) {
  @BeanProperty var aa = DenseMatrix.zeros[Double](m, m)

  /**
    * v为向量x-y，将乘积后的总系数加到aa
    *
    * @param v
    * @param w 本数据点的权重
    */
  private def computeMetrics(v: Vector[Double], w: Double) = {
    val xx = DenseMatrix.zeros[Double](m, m)
    for (i <- 0 until m) {
      for (j <- 0 until m) {
        xx(i, j) = v(i) * v(j)
      }
    }
    aa += xx * w
  }

  /**
    * 直接将两向量用于计算距离总和
    *
    * @param x
    * @param y
    * @param w
    */
  def addToADistance(x: Vector[Double], y: Vector[Double], w: Double): Unit = {
    //计算x、y之差
    val cc = x - y
    this.computeMetrics(cc, w)
  }

  /**
    * 将C输出（点乘后A的系数）
    *
    * @return
    */
  def exportMatrix: Matrix[Double] = {
    val mx = DenseMatrix.zeros[Double](m, m)
    for (i <- 0 until m) {
      for (j <- 0 until m) {
        if (i == j) {
          mx(i, j) = aa(i, j)
        } else {
          mx(i, j) = (aa(i, j) + aa(j, i)) / 2
        }
      }
    }
    mx
  }


}

