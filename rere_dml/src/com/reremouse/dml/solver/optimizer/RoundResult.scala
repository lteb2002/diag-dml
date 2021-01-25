package com.reremouse.dml.solver.optimizer

import breeze.linalg.DenseVector

/**
  *
  */
object RoundResult {

  //四舍五入位数
  val roundNum = 6

  /**
    * 将向量元素四舍五入
    *
    * @param res
    */
  def apply(res: DenseVector[Double]): DenseVector[Double] = {
    this.apply(res, roundNum)
  }

  /**
    *
    * @param res
    * @param rn
    * @return
    */
  def apply(res: DenseVector[Double], rn: Int): DenseVector[Double] = {
    val d = new DenseVector[Double](res.length)
    for (i <- 0 until res.length) {
      d(i) = RoundResult(res(i), rn)
    }
    d
  }

  /**
    * 将小数四舍五入
    *
    * @param res
    * @return
    */
  def apply(res: Double): Double = {
    this.apply(res, roundNum)
  }

  /**
    *
    * @param res
    * @param rn
    * @return
    */
  def apply(res: Double, rn: Int): Double = {
    var r = res
    //防止为无穷大或者非数
    if (r.isNaN) r = 0 else if (r.isInfinity) r = Double.MaxValue
    val b = new java.math.BigDecimal(r)
    val d = b.setScale(rn, java.math.BigDecimal.ROUND_HALF_UP).doubleValue()
    d
  }

}
