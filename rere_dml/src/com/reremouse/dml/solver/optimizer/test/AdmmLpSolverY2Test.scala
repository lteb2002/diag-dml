package com.reremouse.dml.solver.optimizer.test

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import com.reremouse.dml.solver.optimizer.admm.lp.{LambdaUpdaterLpY2, X1UpdaterLpY2, X2UpdaterLpY2}
import com.reremouse.dml.solver.optimizer.admm.{ConstrainedErrorFunction, RereAdmm}

/**
  * 本类使用ADMM与乘子法求解线性规划问题
  */
object AdmmLpSolverY2Test {

  def main(args: Array[String]): Unit = {

    System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "warning")

    val x1 = DenseVector[Double](1.0, 1.0, 1.0)
    val x2 = DenseVector[Double](1.0, 1.0, 1.0)
    val lambda = DenseVector.ones[Double](3) + 0.5

    val c1 = DenseVector[Double](-3, -1.0, 0.0)
    val c2 = DenseVector[Double](-2.0, 0.0, 0)
    val b = DenseVector[Double](30, 24, 36)

    val A1 = DenseMatrix(
      (1.0, 1.0, 1.0),
      (2.0, 2.0, 0.0),
      (4.0, 1.0, 0.0))

    val A2 = DenseMatrix(
      (3.0, 0.0, 0.0),
      (5.0, 1.0, 0.0),
      (2.0, 0.0, 1.0))

    var beta: Double = 2

    var constrainError: DenseVector[Double] = A1 * x1 + A2 * x2 - b

    val upx1 = new X1UpdaterLpY2(c1, c2, A1, A2, b)
    val upx2 = new X2UpdaterLpY2(c1, c2, A1, A2, b)
    val uplambda = new LambdaUpdaterLpY2(A1, A2, b)
    val cef = new ConstrainedErrorFunction {
      override def calculate(x10: DenseVector[Double], x20: DenseVector[Double]): Double = {
        norm(A1 * (x10*:*x10) + A2 * (x20*:*x20) - b)
      }
    }

    val start = System.currentTimeMillis()
    val admm = new RereAdmm(x1, x2, c1, c2, A1, A2, b, lambda, 0.01, upx1, upx2, uplambda, cef)
    val re = admm.solve()
    val end = System.currentTimeMillis()
    val p1=re._1 *:* re._1
    val p2=re._2 *:* re._2
    val obj=c1.t*p1+c2.t*p2
    println("result,p1:" + p1+ ",p2:" + p2+",obj:"+obj)
    println("Time cost:"+(end-start)/1000.0+"s")
  }

}
