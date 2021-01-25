package com.reremouse.dml.solver.optimizer

import breeze.linalg.{DenseMatrix, DenseVector, norm}

/**
  * DFP拟牛顿法的实现
  */
class DFP {


  var gamma: Double = 0.01 //步长因子
  var maxStep: Double = 1.0E5 //最大步数
  var ifFixedStep: Boolean = false
  var ifPositiveValue: Boolean = false
  var precision: Double = 1.0E-5

  /**
    *
    * @param precision       精度
    * @param stepSize        步长因子
    * @param maxStep         最大迭代步数
    * @param ifFixedStep     步长因子是否固定
    * @param ifPositiveValue 是否必须为正值
    */
  def this(precision: Double = 1.0E-5,
           stepSize: Double = 0.01,
           maxStep: Double = 1.0E5,
           ifFixedStep: Boolean = false,
           ifPositiveValue: Boolean = false) = {
    this()
    this.precision = precision
    this.gamma = stepSize
    this.maxStep = maxStep
    this.ifPositiveValue = ifPositiveValue
    this.ifFixedStep = ifFixedStep
  }


  var g: DenseVector[Double] = null
  var H: DenseMatrix[Double] = null
  var p: DenseVector[Double] = null
  var q: DenseVector[Double] = null

  var d: DenseVector[Double] = null
  var lambda: Double = 0.01

  var ifFirst: Boolean = true


  /**
    * 计算更新步中的d(方向)
    *
    * @param curX
    * @param df
    * @return
    */
  private def computeD(curX: DenseVector[Double], df: MathFunction, args: Array[Any]): DenseVector[Double] = {
    if (ifFirst) {
      g = df.computeGradient(curX, args)
      //println("g:"+g)
      ifFirst = false
    } else {
      val g2 = df.computeGradient(curX, args)
      //println("g2:"+g2)
      p = lambda * d
      //println("p:"+p)
      q = g2 - g
      //println("q:"+q)
      g = g2
      //如果梯度无变化，结束迭代
      if (norm(q) == 0) {
        H = DenseMatrix.zeros(H.rows, H.cols)
      } else {
        H = H + (p * p.t) / (p.t * q) - (H * q * q.t * H) / (q.t * H * q)
      }

    }
    d = -H * g
    d
  }

  /**
    * 计算更新步中的lambda(步长)
    *
    * @param curX
    * @param df
    * @param d
    * @return
    */
  private def computeLambda(curX: DenseVector[Double], df: MathFunction, d: DenseVector[Double], args: Array[Any]): Double = {
    val df2 = new MathFunction {
      override def computeObjective(lam: DenseVector[Double], args: Array[Any]): Double = {
        df.computeObjective(curX + lam(0) * d, args)
      }

      override def computeGradient(lam: DenseVector[Double], args: Array[Any]): DenseVector[Double] = {
        var temp = df.computeGradient(curX + lam(0) * d, args).t * d
        //println("lambda:" + lambda + ",temp:" + temp)
        //println(temp)
        val dv = DenseVector.zeros[Double](1)
        dv(0) = temp
        dv
      }
    }
    val lam0 = DenseVector.ones[Double](1)
    lam0(0) = 0.01
    lambda = new SteepestDescent().gredientDescent(lam0, df2, args = args)(0)
    //if(lambda<0)lambda=0.001
    lambda
  }


  /**
    *
    * @param curX
    * @param df
    * @return
    */
  def iterate(curX: DenseVector[Double],
              df: MathFunction,
              args: Array[Any]): Tuple2[DenseVector[Double], Double] = {
    //println("curX:" + curX)
    d = this.computeD(curX, df, args)
    //lambda=if(ifFixedStep)gamma else this.computeLambda(curX, df, d,args)
    lambda = if (ifFixedStep) gamma else new ArmijoSearch().search(curX, df, d, args)
    //println("d:" + d)
    //println("lambda:" + lambda)
    var newX = curX + lambda * d
    val stepSize: Double = norm(newX - curX)
    newX = newX.map((el: Double) => {
      var newV = el
      if (el < 0 && ifPositiveValue) {
        newV = Double.MinPositiveValue
      }
      if (el.isNaN || el.isInfinity) {
        newV = Double.MaxValue
      }
      newV
    })
    (newX, stepSize)
  }


  def gredientDescent(curX: DenseVector[Double],
                      df: MathFunction,
                      args: Array[Any] = Array[Any]()): DenseVector[Double] = {
    var (newX, error) = (curX, 0.0)
    H = DenseMatrix.eye(curX.length)
    var step = 1
    try {
      do {
        val re = this.iterate(newX, df, args)
        newX = re._1
        error = re._2
        step += 1
      }
      while (error > precision && step <= maxStep)
      //println("The local minimum is reached by DFP at " + newX)
      //println("Total steps:" + step)
      newX
    } catch {
      case e: Exception =>
        e.printStackTrace()
        println(newX)
        newX
    }

  }
}

object DFP {
  def main(args: Array[String]): Unit = {

    val df = new MathFunction() {
      override def computeObjective(x: DenseVector[Double], args: Array[Any]): Double = {
        val xx: DenseVector[Double] = x + 1.0
        xx.t * xx
      }

      override def computeGradient(x: DenseVector[Double], args: Array[Any]): DenseVector[Double] = {
        val dfx = (x * (2.0)) + 2.0
        dfx
      }
    }

    val curX = DenseVector(6.0, 6.0)
    val precision = 0.00001
    val previousStepSize = 1 / precision
    val start=System.currentTimeMillis()
    val method = new DFP(ifFixedStep = false)
    val ans = method.gredientDescent(curX, df)
    val end=System.currentTimeMillis()
    println("Time:"+(end-start)/1000.0)
    println(ans)

  }
}

