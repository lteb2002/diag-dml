package com.reremouse.dml.solver.optimizer

import breeze.linalg.{DenseVector, norm}

/**
  * 最速下降法
  * 优点：计算量小，在简单问题上收敛快
  * 缺点：在大梯度的陡峭曲面上，容易有过大的梯度（梯度爆炸），优化点容易行走过远，导致剧烈振荡，甚至NaN的情况；
  * Armijo搜索步长能部分改善这种问题
  */
class SteepestDescent {


  var gamma: Double = 0.001 //步长因子
  var maxStep: Double = 1.0E5 //最大步数
  var ifFixedStep: Boolean = false
  var ifPositiveValue: Boolean = false
  var precision: Double = 1.0E-4

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
    * @param curX
    * @param df
    * @param args
    * @return
    */
  def computeStdDir(curX: DenseVector[Double],df: MathFunction,args: Array[Any] ):DenseVector[Double]={
    val gra = df.computeGradient(curX, args)
    val graV = norm(gra)
    //避免梯度爆炸，出现NaN，但会导致收敛性不好
    var stdDir=if (graV < Float.MaxValue) gra else gra/graV
    stdDir
  }

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
    //var dir = -df.computeGradient(curX, args)
    var dir = -this.computeStdDir(curX,df, args)
    //var dir = if (ifFixedStep) -this.computeStdDir(curX,df, args) else -df.computeGradient(curX, args)
    //println("d:"+dir)
    //最速下降法搜索方向为负梯度
    //dir = dir/norm(dir)
    //搜索最佳步长
    val lambda = if (ifFixedStep) this.gamma else new ArmijoSearch().search(curX, df, dir, args)
    //println("lambda:"+lambda)
    //val lambda = this.gamma
    var newX = curX + lambda * dir
    val error: Double = norm(dir)
    newX = newX.map((el: Double) => {
      var newV = el
      if (el < 0 && ifPositiveValue) {
        newV = 0.01
      }
      if (el.isNaN || el.isInfinity) {
        newV = Double.MaxValue
      }
      newV
    })
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
        //
        if (testIfNaN(newX)) {
          println("There is a NaN:" + newX)
          throw new Exception("new NaN")
        }
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

  def testIfNaN(newX: DenseVector[Double]): Boolean = {
    var ifNaN = false
    for (i <- 0 until newX.length) {
      if (newX(i).isNaN) ifNaN = true
    }
    ifNaN
  }

}

object SteepestDescent {


  def main(args: Array[String]): Unit = {
    val df = new MathFunction() {
      override def computeObjective(x: DenseVector[Double], args: Array[Any] = Array[Any]()): Double = {
        val xx: DenseVector[Double] = x + 1.0
        xx.t * xx
      }

      override def computeGradient(x: DenseVector[Double], args: Array[Any] = Array[Any]()): DenseVector[Double] = {
        val dfx = (x * (2.0)) + 2.0
        dfx
      }
    }

    val curX = DenseVector(6.0, 6.0)
    val precision = 0.00001
    val previousStepSize = 1 / precision
    val start = System.currentTimeMillis()
    val method = new SteepestDescent(ifFixedStep = true)
    val ans = method.gredientDescent(curX, df)
    val end = System.currentTimeMillis()
    println("Time:" + (end - start) / 1000.0)
    println(ans)
  }
}