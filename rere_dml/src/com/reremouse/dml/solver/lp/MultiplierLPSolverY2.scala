package com.reremouse.dml.solver.lp

import java.util

import breeze.linalg._
import breeze.numerics.{abs, pow, sqrt}
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MultiplierMethod, RoundResult}
import com.reremouse.util.RereLogger
import org.apache.commons.math3.optim.linear._
import scpsolver.constraints.LinearBiggerThanEqualsConstraint
import scpsolver.lpsolver.SolverFactory
import scpsolver.problems.LinearProgram

/**
  * 使用乘子法、内点法求解线性规划问题
  * Created by RereMouse on 2018-01-18.
  * Just for test purpose
  */
@Deprecated
class MultiplierLPSolverY2 extends RereLPSolver {
  val logger = RereLogger.getLogger(this.getClass)
  //正则项权重
  //val regWeight: Double = 10
  /**
    * 求解线性规划问题（最小化目标函数，大于等于约束）
    *
    * @param obj   目标函数系统
    * @param const 约束矩阵
    * @param b     约束值
    * @return 取得线性规划的解
    */
  def solve(obj: DenseVector[Double], const: DenseMatrix[Double], b: DenseVector[Double],
            regType: String = "L2", regWeight: Double,
            mainVarLen: Int = 0): (Double, DenseVector[Double]) = {
    if (obj.length == const.cols && const.rows == b.length) {
      val ree = LPCoordinator.transformBiggerEqualsThanAsEquals(const, obj)
      val A = ree._1
      val c = ree._2
      println("Variable number:" + A.cols + ", Constrain number:" + A.rows)
      //val A=DenseMatrix.horzcat(const,-DenseMatrix.eye[Double](const.rows))
      //val c:DenseVector[Double]=DenseVector.vertcat(obj,DenseVector.zeros[Double](A.rows))
      val x0 = DenseVector.ones[Double](c.length)
      val lambda = DenseVector.ones[Double](A.rows) + 0.5
      val beta: Double = 2.0

      val df = new ConstrainedMathFunction() {
        override def computeObjective(x: DenseVector[Double], args: Array[Any]): Double = {
          val newV: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
          val newTho: Double = args(1).asInstanceOf[Double]
          var reg: Double = 0.0
          if (regType.toLowerCase().equals("l1")) {
            reg = sum(abs(x))
          } else if (regType.toLowerCase().equals("l2")) {
            reg = sum(pow(x, 2.0))
          }
          c.t * (x *:* x) - newV.t * (A * (x *:* x) - b) + (newTho / 2.0) * ((A * (x *:* x) - b).t * (A * (x *:* x) - b)) + regWeight * reg
        }

        override def computeGradient(xx: DenseVector[Double], args: Array[Any]): DenseVector[Double] = {
          val newV: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
          val newTho: Double = args(1).asInstanceOf[Double]
          var reg: DenseVector[Double] = DenseVector.zeros[Double](xx.length)
          if (regType.toLowerCase().equals("l1")) {
            for (i <- 0 until xx.length) {
              reg(i) = if (xx(i) > 0) 1.0 else -1.0
            }
            reg = abs(DenseVector.ones[Double](xx.length))
          } else if (regType.toLowerCase().equals("l2")) {
            reg = sum(xx *:* xx) * xx * 4.0
          }
          val df1 = 2.0 * (c *:* xx) - (2.0 * A.t * newV) *:* xx + (2 * newTho * A.t * (A * (xx *:* xx) - b)) *:* xx + regWeight * reg
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
      val min: Double = re._2
      val end = System.currentTimeMillis()
      println("Time cost:" + (end - start) / 1000.0 + "s")
      val value = ans *:* ans
      logger.info("Minimum value: " + min + "\n")
      //print decision variables
      logger.info("Solution:\n" + value)
      (RoundResult(min), RoundResult(value))
    } else {
      logger.error("The length of the obj is not equal to the columns of constraints or the rows of constraints are not equal to the length of b.")
      (0, DenseVector.ones[Double](obj.length))
    }
  }

}
