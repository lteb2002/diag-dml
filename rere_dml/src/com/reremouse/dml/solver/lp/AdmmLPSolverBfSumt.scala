package com.reremouse.dml.solver.lp

import breeze.linalg._
import breeze.numerics.{abs, log, pow, sqrt}
import com.reremouse.dml.solver.optimizer.admm.{ConstrainedErrorFunction, RereAdmm}
import com.reremouse.dml.solver.optimizer.admm.lp._
import com.reremouse.dml.solver.optimizer.test.AdmmLpSolverBfSumtTest
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MultiplierMethod}
import com.reremouse.util.RereLogger

/**
  * Created by RereMouse on 2018-01-18.
  * 使用ADMM与Sumit方法求解(Bf表示障碍函数)
  * Just for test purpose
  */
@Deprecated
class AdmmLPSolverBfSumt extends RereLPSolver {
  val logger = RereLogger.getLogger(this.getClass)
  var mainVnum: Int = 0
  var s1Vnum: Int = 0
  var s2Vnum: Int = 0
  var tolerance:Double =1.0E-4

  //Sumt方法中的u系数，迭代中逐渐变小
  var u: Double = 1.0E2

  def this(mainVnum: Int = 0, s1Vnum: Int = 0, s2Vnum: Int = 0,
           tolerance0:Double=1.0E-4) {
    this()
    this.mainVnum = mainVnum
    this.s1Vnum = s1Vnum
    this.s2Vnum = s2Vnum
    this.tolerance=tolerance0
  }

  /**
    * 求解线性规划问题（最小化目标函数，大于等于约束）
    *
    * @param obj   目标函数系统
    * @param const 约束矩阵
    * @param b     约束值
    * @return 取得线性规划的解
    */
  def solve(obj: DenseVector[Double],
            const: DenseMatrix[Double],
            b: DenseVector[Double],
            regType: String = "l2",
            regWeight:Double,
            mainVarLen: Int = 0
           ): (Double, DenseVector[Double]) = {
    if (obj.length == const.cols && const.rows == b.length) {
      val ree = LPCoordinator.transformBiggerEqualsThanAsAdmmSegments(const, obj, mainVnum, s1Vnum, s2Vnum)
      val A1 = ree._1
      val A2 = ree._2
      val c1: DenseVector[Double] = ree._3
      val c2: DenseVector[Double] = ree._4

      val m1Num = ree._5
      val m2Num = ree._6


      val x1 = DenseVector.ones[Double](c1.length) + 5.0
      val x2 = DenseVector.ones[Double](c2.length) + 5.0
      val lambda = DenseVector.ones[Double](A1.rows) + 0.5


      val upx1 = new X1UpdaterLpBf(c1, c2, A1, A2, b)
      val upx2 = new X2UpdaterLpBf(c1, c2, A1, A2, b)
      val uplambda = new LambdaUpdaterLpBf(A1, A2, b)
      val cef = new ConstrainedErrorFunction {
        override def calculate(x10: DenseVector[Double], x20: DenseVector[Double]): Double = {
          norm(A1 * (x10) + A2 * (x20) - b)
        }
      }

      var p1: DenseVector[Double] = x1
      var p2: DenseVector[Double] = x2
      var error = 1.0
      var step = 0
      val maxStep = 100000000
      val start = System.currentTimeMillis()
      var min = 0.0
      //目标函数值
      do {
        val admm = new RereAdmm(p1, p2, c1, c2, A1, A2, b, lambda, 2, upx1, upx2, uplambda, cef,tolerance,regType,regWeight)
        val re = admm.solve()
        p1 = re._1
        p2 = re._2
        //
        error = abs(-sum(u * log(x1)) - sum(u * log(x2)))
        println("SUMT step:" + step + ",error:" + error + ",obj:" + re._3 + ",u:" + u)
        u = u * 0.3
        step += 1
      } while (error > 2.0E-3 && step < maxStep)

      //
      //      val start = System.currentTimeMillis()
      //      val admm = new RereAdmm(x1, x2, c1, c2, A1, A2, b, lambda, 1.0E-200, upx1, upx2, uplambda, cef)
      //      val re = admm.solve()
      val px1: DenseVector[Double] = p1(0 until m1Num)
      //ADMM第一部分结果
      val px2: DenseVector[Double] = p2(0 until m2Num)
      //ADMM第二部分结果

      val end = System.currentTimeMillis()
      println("Time cost:" + (end - start) / 1000.0 + "s")
      val value = DenseVector.vertcat(px1 , px2 )
      min = c1.t * p1 + c2.t * p2
      logger.info("Minimum value: " + min + "\n")
      //print decision variables
      logger.info("Solution:\n" + value)
      (min, value)
    }
    else {
      logger.error("The length of the obj is not equal to the columns of constraints or the rows of constraints are not equal to the length of b.")
      (0, DenseVector.ones[Double](obj.length))
    }
  }

}
