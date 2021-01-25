package com.reremouse.dml.solver.optimizer.test


import breeze.linalg.{DenseMatrix, DenseVector}
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, DFP, MathFunction, MultiplierMethod, RoundResult}

//测试
/**
  * 测试基于乘子法求解线性规划问题
  */
object LPMultiplierTest {
  def testIfNaN(newX: DenseVector[Double]): Boolean = {
    var ifNaN = false
    for (i <- 0 until newX.length) {
      if (newX(i).isNaN) ifNaN = true
    }
    ifNaN
  }

  def main(args: Array[String]): Unit = {

    val x0 = DenseVector[Double](1.0, 1.0, 1.0, 5, 15, 29.0)

    //val x0 = DenseVector.ones[Double](6)

    val lambda = DenseVector.ones[Double](3) + 0.5
    val beta: Double = 5.0

    val c = DenseVector[Double](-3, -1, -2, 0, 0, 0)
    val b = DenseVector[Double](30, 24, 36)

    val A = DenseMatrix(
      (1.0, 1.0, 3.0, 1.0, 0.0, 0.0),
      (2.0, 2.0, 5.0, 0.0, 1.0, 0.0),
      (4.0, 1.0, 2.0, 0.0, 0.0, 1.0))

    val df = new ConstrainedMathFunction() {

      override def computeObjective(x: DenseVector[Double], args: Array[Any]): Double = {
        val newV: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
        val newTho: Double = args(1).asInstanceOf[Double]
        c.t * (x *:* x) - newV.t * (A * (x *:* x) - b) + (newTho / 2.0) * ((A * (x *:* x) - b).t * (A * (x *:* x) - b))
      }

      override def computeGradient(xx: DenseVector[Double], args: Array[Any]): DenseVector[Double] = {
        val newV: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
        val newTho: Double = args(1).asInstanceOf[Double]
        val df1 = 2.0 * (c *:* xx) - (2.0 * A.t * newV) *:* xx + (2 * newTho * A.t * (A * (xx *:* xx) - b)) *:* xx
        if (testIfNaN(df1)) {
          println("There is a NaN:" + df1)
          println("tho:" + newTho)
          println("v:" + newV)
          println("x:" + xx)
          throw new Exception("new NaN")
        }
        df1
      }

      override def computeConstrainError(xx: DenseVector[Double]): DenseVector[Double] = {
        A * (xx *:* xx) - b
      }
    }
    val start = System.currentTimeMillis()
    val method = new MultiplierMethod(lambda, beta)
    val re = method.gredientDescent(x0, df)
    val ans = re._1
    val obj: Double = re._2
    val args=re._3
    val v: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
    val tho: Double = args(1).asInstanceOf[Double]
    print(RoundResult(ans *:* ans).toString() + ",obj:")
    println(RoundResult(obj))
    println("v:"+v)
    println("tho:"+tho)
    val end = System.currentTimeMillis()
    println("Time cost:" + (end - start) / 1000.0 + "s")

//    val vv = DenseVector(2.828429550863641, -1.999995666327536, 9.0861723316214E-5, 4.242641369448289, 3.247951673850387E-5, 3.837451154488409E-6)
//    println(df.computeObjective(vv))


  }


}
