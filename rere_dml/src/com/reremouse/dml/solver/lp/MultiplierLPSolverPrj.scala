package com.reremouse.dml.solver.lp

import breeze.linalg._
import breeze.numerics.{abs, log, pow}
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MultiplierMethod, RoundResult}
import com.reremouse.util.RereLogger

/**
  * 使用乘子法、内点法求解线性规划问题
  * Created by RereMouse on 2018-01-18.
  */
class MultiplierLPSolverPrj extends RereLPSolver {
  val logger = RereLogger.getLogger(this.getClass)

  //Sumt方法中的u系数，迭代中逐渐变小
  var u: Double = 1.0E2

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
            regType: String = "L2",
            regWeight: Double,
            mainVarLen: Int = 0): (Double, DenseVector[Double]) = {
    if (obj.length == const.cols && const.rows == b.length) {
      val ree = LPCoordinator.transformBiggerEqualsThanAsEquals(const, obj)
      //val ree = LPCoordinator.transformLessEqualsThanAsEquals(const, obj)
      val A = ree._1
      val c = ree._2
      println(A)
      println(c)
      println("Variable number:" + A.cols + ", Constrain number:" + A.rows)
      //val A=DenseMatrix.horzcat(const,-DenseMatrix.eye[Double](const.rows))
      //val c:DenseVector[Double]=DenseVector.vertcat(obj,DenseVector.zeros[Double](A.rows))
      var x0 = DenseVector.ones[Double](c.length)
      val lambda = DenseVector.ones[Double](A.rows) + 0.5
      val beta: Double = 5.0

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
          c.t * x  + newV.t * (A * x - b) + (newTho / 2.0) * ((A * x - b).t * (A * x - b)) + regWeight * reg
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
            reg = xx * 2.0
          }
          val df1 = c + A.t * newV + newTho * A.t * (A * xx - b) + regWeight * reg
          df1
        }

        override def computeConstrainError(xx: DenseVector[Double]): DenseVector[Double] = {
          A * (xx) - b
        }
      }
      val maxStep = 100000000
      var error = 1.0
      var step = 0
      val start = System.currentTimeMillis()
      var ans = DenseVector.ones[Double](obj.length)
      var min: Double = 0
      val method = new MultiplierMethod(lambda, beta)
      val re = method.gredientDescent(x0, df, ifPositiveValue = false)
      ans = re._1
      x0 = ans
      min = re._2
      val end = System.currentTimeMillis()
      println("Time cost:" + (end - start) / 1000.0 + "s")
      logger.info("Minimum value: " + min + "\n")
      //print decision variables
      logger.info("Solution:\n" + ans)
      (RoundResult(min), RoundResult(ans))
    } else {
      logger.error("The length of the obj is not equal to the columns of constraints or the rows of constraints are not equal to the length of b.")
      (0, DenseVector.ones[Double](obj.length))
    }
  }

}
