package com.reremouse.dml.solver.lp

import breeze.linalg._
import breeze.numerics.{abs, log}
import com.reremouse.dml.solver.optimizer.admm.lp._
import com.reremouse.dml.solver.optimizer.admm.{ConstrainedErrorFunction, RereAdmm}
import com.reremouse.util.RereLogger

/**
  * Created by RereMouse on 2018-01-18.
  * 使用ADMM与Sumit方法求解
  */
class AdmmLPSolverY2 extends RereLPSolver {
  val logger = RereLogger.getLogger(this.getClass)
  var mainVnum: Int = 0
  var s1Vnum: Int = 0
  var s2Vnum: Int = 0

  def this(mainVnum: Int, s1Vnum: Int, s2Vnum: Int) {
    this()
    this.mainVnum = mainVnum
    this.s1Vnum = s1Vnum
    this.s2Vnum = s2Vnum
  }

  /**
    * 求解线性规划问题（最小化目标函数，大于等于约束）
    *
    * @param obj   目标函数系统
    * @param const 约束矩阵
    * @param b     约束值
    * @return 取得线性规划的解
    */
  def solve(obj: DenseVector[Double], const: DenseMatrix[Double], b: DenseVector[Double],
            regType: String = "l2", regWeight:Double,
            mainVarLen: Int = 0): (Double, DenseVector[Double]) = {
    if (obj.length == const.cols && const.rows == b.length) {
      val ree = LPCoordinator.transformBiggerEqualsThanAsAdmmSegments(const, obj, mainVnum, s1Vnum, s2Vnum)
      println("b:" + b)
      val A1 = ree._1
      val A2 = ree._2
      val c1: DenseVector[Double] = ree._3
      val c2: DenseVector[Double] = ree._4
      val m1Num = ree._5 //主变量第一部分长度
      val m2Num = ree._6 //主变量第二部分长度
      val x1 = DenseVector.ones[Double](c1.length)+ 0.5//x1初始值
      val x2 = DenseVector.ones[Double](c2.length)+ 0.5//x2初始值
      val lambda = DenseVector.ones[Double](A1.rows) + 0.5//lambda初始值

      val upx1 = new X1UpdaterLpY2(c1, c2, A1, A2, b)//x1更新器
      val upx2 = new X2UpdaterLpY2(c1, c2, A1, A2, b)//x2更新器
      val uplambda = new LambdaUpdaterLpY2(A1, A2, b)//lambda更新器

      val cef = new ConstrainedErrorFunction {//计算ADMM误差
        override def calculate(x10: DenseVector[Double], x20: DenseVector[Double]): Double = {
          norm(A1 * (x10 *:* x10) + A2 * (x20 *:* x20) - b)
        }
      }

      val start = System.currentTimeMillis()
      val admm = new RereAdmm(x1, x2, c1, c2, A1, A2, b, lambda, 0.01, upx1, upx2, uplambda, cef)
      val re = admm.solve()
      val p1 = re._1 *:* re._1//变为x=y平方
      val p2 = re._2 *:* re._2//变为x=y平方

      //ADMM第一部分结果
      val px1: DenseVector[Double] = p1(0 until m1Num)
      //ADMM第二部分结果
      val px2: DenseVector[Double] = p2(0 until m2Num)

      var min: Double = c1.t * (p1 *:* p1) + c2.t * (p2 *:* p2)
      //目标函数值
      val end = System.currentTimeMillis()
      println("Time cost:" + (end - start) / 1000.0 + "s")
      val value = DenseVector.vertcat(px1, px2)
      logger.info("Minimum value: " + min + "\n")
      //print decision variables
      logger.info("Solution:\n" + value)
      (min, value)
    } else {
      logger.error("The length of the obj is not equal to the columns of constraints or the rows of constraints are not equal to the length of b.")
      (0, DenseVector.ones[Double](obj.length))
    }
  }

}
