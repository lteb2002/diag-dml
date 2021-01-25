package com.reremouse.dml.solver.optimizer.test

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import breeze.numerics.log
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MultiplierMethod, RoundResult}

//测试
/**
  * 测试基于乘子法求解线性规划问题
  */
object LPMultiplierTest2 {
  def testIfNaN(newX: DenseVector[Double]): Boolean = {
    var ifNaN = false
    for (i <- 0 until newX.length) {
      if (newX(i).isNaN) ifNaN = true
    }
    ifNaN
  }

  def main(args: Array[String]): Unit = {

    val x0 = DenseVector[Double](1.0, 1.0, 1.0, 1, 1, 1)

    //val x0 = DenseVector.ones[Double](6)

    val lambda = DenseVector.ones[Double](3) + 0.5
    val beta: Double = 5.0

    val c = DenseVector[Double](-3, -1, -2, 0, 0, 0)
    val b = DenseVector[Double](30, 24, 36)

    val A = DenseMatrix(
      (1.0, 1.0, 3.0, 1.0, 0.0, 0.0),
      (2.0, 2.0, 5.0, 0.0, 1.0, 0.0),
      (4.0, 1.0, 2.0, 0.0, 0.0, 1.0))

    val u=0.02

    val df = new ConstrainedMathFunction() {

      override def computeObjective(x: DenseVector[Double], args: Array[Any]): Double = {
        val newV: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
        val newTho: Double = args(1).asInstanceOf[Double]
        c.t * x  - u* sum(log(x)) - newV.t * (A * x  - b) + (newTho / 2.0) * ((A * x - b).t * (A * x - b))
      }

      override def computeGradient(xx: DenseVector[Double], args: Array[Any]): DenseVector[Double] = {
        val newV: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
        val newTho: Double = args(1).asInstanceOf[Double]
        val df1 = c  - u/(xx ) -  A.t * newV + newTho * A.t * (A * xx  - b)
        if (testIfNaN(df1)) {
          println("There is a NaN:" + df1)
          println("tho:"+newTho)
          println("v:"+newV)
          println("x:"+xx)
          throw new Exception("new NaN")
        }
        df1
      }

      override def computeConstrainError(xx: DenseVector[Double]): DenseVector[Double] = {
        A * (xx) - b
      }
    }
    val start = System.currentTimeMillis()
    val method = new MultiplierMethod(lambda,beta)
    val re=method.gredientDescent(x0, df)
    val ans = re._1
    val obj:Double=re._2
    print(RoundResult(ans*:*ans).toString()+",obj:")
    println(RoundResult(obj))
    val end = System.currentTimeMillis()
    println("Time cost:"+(end-start)/1000.0+"s")
  }

}
