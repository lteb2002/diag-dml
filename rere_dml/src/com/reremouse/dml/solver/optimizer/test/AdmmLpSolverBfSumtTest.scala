package com.reremouse.dml.solver.optimizer.test

import breeze.linalg.{DenseMatrix, DenseVector}
import com.reremouse.dml.solver.lp.AdmmLPSolverBfSumt

/**
  * 本类使用ADMM与SUMIT方法（内点法）求解线性规划问题
  */
class AdmmLpSolverBfSumtTest {

}


object AdmmLpSolverBfSumtTest {
  var u: Double = 100

  def main(args: Array[String]): Unit = {
    //val x1 = DenseVector[Double](1.0, 1.0, 5.0)
    //val x2 = DenseVector[Double](1.0, sqrt(15.0), sqrt(29.0))
    val x1 = DenseVector[Double](1.0, 1.0, 1.0)
    val x2 = DenseVector[Double](1.0, 1.0, 5.0)
//
//    val lambda = DenseVector.ones[Double](3) + 0.5
//
//    val c1 = DenseVector[Double](-3, -1, 0)
//    val c2 = DenseVector[Double](-2, 0, 0)
//    val b = DenseVector[Double](30, 24, 36)
//
//    val A1 = DenseMatrix(
//      (1.0, 1.0, 1.0),
//      (2.0, 2.0, 0.0),
//      (4.0, 1.0, 0.0))
//
//    val A2 = DenseMatrix(
//      (3.0, 0.0, 0.0),
//      (5.0, 1.0, 0.0),
//      (2.0, 0.0, 1.0))

    val c = DenseVector[Double](-3, -1, -2)
    val b = DenseVector[Double](30, 24, 36)

    val A = DenseMatrix(
      (-1.0, -1.0, -3.0),
      (-2.0, -2.0, -5.0),
      (-4.0, -1.0, -2.0))

    val solver =new AdmmLPSolverBfSumt(3,0,3)
    val result=solver.solve(c,A,b,"L2",1.0E-5)
    println(result)

//    val upx1 = new X1UpdaterLpBf(c1, c2, A1, A2, b)
//    val upx2 = new X2UpdaterLpBf(c1, c2, A1, A2, b)
//    val uplambda = new LambdaUpdaterLpBf(A1, A2, b)
//    val cef = new ConstrainedErrorFunction {
//      override def calculate(x10: DenseVector[Double], x20: DenseVector[Double]): Double = {
//        norm(A1 * (x10) + A2 * (x20) - b)
//      }
//    }
//    var p1: DenseVector[Double] = x1
//    var p2: DenseVector[Double] = x2
//    var obj: Double = 0
//    var error = 1.0
//    var error2 = 1.0
//    var step = 0
//    val maxStep = 100000000
//   val start = System.currentTimeMillis()
//    do {
//      val admm = new RereAdmm(p1, p2, c1, c2, A1, A2, b, lambda, 2, upx1, upx2, uplambda, cef)
//      val re = admm.solve()
//      p1 = re._1
//      p2 = re._2
//      error = abs(-sum(AdmmLpSolverBfSumtTest.u * log(x1)) - sum(AdmmLpSolverBfSumtTest.u * log(x2)))
//      println("SUMT step:" + step + ",error:" + error + ",obj:" + re._3 + ",u:" + u)
//      u = u * 0.3
//      step += 1
//    } while (error > 2.0E-3 && step < maxStep)
//

    //obj = c1.t * p1 + c2.t * p2
    //println("result,p1:" + p1 + ",p2:" + p2 + ",obj:" + obj)

//   val end = System.currentTimeMillis()
//   println("Time cost:" + (end - start) / 1000.0 + "s")
  }

}
