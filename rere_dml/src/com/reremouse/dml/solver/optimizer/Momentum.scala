package com.reremouse.dml.solver.optimizer

import breeze.linalg.{DenseVector, norm, sum}
import breeze.numerics.abs

class Momentum {


  var gamma: Double = 1.0E-4 //步长因子
  var maxStep: Double = 1.0E20 //最大步数
  var ifFixedStep: Boolean = false
  var ifPositiveValue: Boolean = false
  var precision: Double = 1.0E-4

  val r = 0.9 //动量因子

  /**
    *
    * @param precision       精度
    * @param stepSize        步长因子
    * @param maxStep         最大迭代步数
    * @param ifFixedStep     步长因子是否固定
    * @param ifPositiveValue 是否必须为正值
    */
  def this(precision: Double = 1.0E-4,
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

  /**
    * 计算梯度方向，消除梯度大小本身的影响
    * 防止梯度爆炸和梯度消失
    *
    * @param curX
    * @param df
    * @param args
    * @return
    */
  def computeStdDir(curX: DenseVector[Double], df: MathFunction, args: Array[Any]): DenseVector[Double] = {
    val gra = df.computeGradient(curX, args)
    val graV = norm(gra)
    val dir=if (graV == 0) gra else gra / graV
    //val dir=gra
   dir
  }

  //动量，等循环更新使用
  var curV: DenseVector[Double] = null

  /**
    *
    * @param curX
    * @param df
    * @param args
    * @return
    */
  private def iterate(curX: DenseVector[Double],
                      df: MathFunction,
                      args: Array[Any] = Array[Any]()): Tuple2[DenseVector[Double], Double] = {
    //println(curX)
    //var dir = df.computeGradient(curX, args)
    //println("d:"+dir)
    //最速下降法搜索方向为负梯度
    //dir = dir/norm(dir)
    val stdGra = this.computeStdDir(curX, df, args)
    //搜索最佳步长
    var lambda = if (ifFixedStep) {
      this.gamma
    } else {
      new ArmijoSearch().search(curX, df, -stdGra, args)
    }
    //val lambda = this.gamma
    //println("lambda:"+lambda)
    //val lambda = this.gamma
    //移动的方向
    var dir: DenseVector[Double] = null
    if (curV == null) {
      dir = stdGra
      curV = lambda * dir
    } else {
      dir = this.computeStdDir(curX - r * curV, df, args)
      //动量更新法则：v_t=r*v_(t-1)+lambda*gradient
      curV = r * curV + lambda * dir
    }
    var newX = curX - curV
    val error: Double = norm(dir)
    newX = newX.map((el: Double) => {
      var newV = el
      if (el < 0 && ifPositiveValue) {
        newV = 0
      }
      if (el.isNaN || el.isInfinity) {
        newV = Double.MaxValue
      }
      newV
    })
    //    for (i <- 0 until newX.length) {
    //      if (newX(i) < 0 && ifPositiveValue) {
    //        newX(i) = Double.MinPositiveValue
    //      }
    //      if(newX(i).isNaN||newX(i).isInfinity)
    //        newX(i) = 0
    //    }
    (newX, error)
  }


  def gredientDescent(initX: DenseVector[Double],
                      df: MathFunction,
                      args: Array[Any] = Array[Any]()
                     ): DenseVector[Double] = {
    //println(curX)
    var newX: DenseVector[Double] = initX
    var error: Double = 0
    var step = 1
    try {
      do {
        val re = this.iterate(newX, df, args)
        newX = re._1
        error = re._2
        step += 1
      } while (error > precision && step <= maxStep)
      //println("The local minimum occurs by SteepestDescent at " + newX + ", steps:" + step)
      //println("Total steps:" + step)
      newX
    } catch {
      case e: Exception =>
        e.printStackTrace()
        //println(newX)
        newX
    }
  }
}


object Momentum {
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
    val start = System.currentTimeMillis()
    val method = new Momentum(ifFixedStep = false)
    val ans = method.gredientDescent(curX, df)
    val end = System.currentTimeMillis()
    println("Time:" + (end - start) / 1000.0)
    println(ans)

  }
}
