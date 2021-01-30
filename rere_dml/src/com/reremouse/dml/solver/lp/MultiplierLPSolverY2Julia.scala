package com.reremouse.dml.solver.lp

import breeze.linalg._
import breeze.numerics.{abs, pow}
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MultiplierMethod, RoundResult}
import com.reremouse.ju4ja.client.Ju4jaClient
import com.reremouse.ju4ja.parser.Ju4jaParser
import com.reremouse.util.RereLogger

/**
  * 使用乘子法、内点法求解线性规划问题
  * Created by RereMouse on 2018-01-18.
  * Just for test purpose
  */
@Deprecated
class MultiplierLPSolverY2Julia extends RereLPSolver {
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
      val start = System.currentTimeMillis()
      val jc: Ju4jaClient = new Ju4jaClient("127.0.0.1", 6996)
      val aa =Array.ofDim[Double](A.rows,A.cols)
      for(i <- 0 until A.rows){
        aa(i) = A(i,::).t.toArray
      }
      var args = Array[AnyRef](c.toArray, aa, b.toArray, regType, new java.lang.Double(regWeight))
      val res = jc.invokeFunction("solveDmlLp", "RereDmlLpSolverY2", args)
      var ds: Array[java.lang.Double] = Ju4jaParser.parseStringAs1DArray(res.getResultStr.toString)
      //import scala.collection.JavaConverters._
      var dss = new Array[Double](ds.length)
      for (i <- 0 until dss.length) {
        dss(i) = ds(i)
      }
      val value = new DenseVector[Double](dss)
      val end = System.currentTimeMillis()
      println("Time cost:" + (end - start) / 1000.0 + "s")

      logger.info("Minimum value: " + min + "\n")
      //print decision variables
      logger.info("Solution:\n" + value)
      (0, RoundResult(value))
    } else {
      logger.error("The length of the obj is not equal to the columns of constraints or the rows of constraints are not equal to the length of b.")
      (0, DenseVector.ones[Double](obj.length))
    }
  }

}
