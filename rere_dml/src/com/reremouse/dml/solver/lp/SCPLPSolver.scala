package com.reremouse.dml.solver.lp

import java.util

import breeze.linalg._
import com.reremouse.util.RereLogger
import org.apache.commons.math3.optim.linear._
import scpsolver.constraints.LinearBiggerThanEqualsConstraint
import scpsolver.lpsolver.{LinearProgramSolver, SolverFactory}
import scpsolver.problems.LinearProgram

/**
  * Created by RereMouse on 2018-01-18.
  */
class SCPLPSolver extends RereLPSolver {
  val logger = RereLogger.getLogger(this.getClass)



  //正则项权重
  //val regWeight: Double = 10000

  /**
    * 求解线性规划问题（最小化目标函数，大于等于约束）
    *
    * @param obj0   目标函数系统
    * @param const 约束矩阵
    * @param b     约束值
    * @return 取得线性规划的解
    */
  override def solve(obj0: DenseVector[Double],
                     const: DenseMatrix[Double],
                     b: DenseVector[Double],
                     regType: String = "L1",
                     regWeight:Double,
                     mainVarLen: Int = 0): (Double, DenseVector[Double]) = {
    var obj = obj0
    if (obj0.length == const.cols && const.rows == b.length) {
      if (!"none".equals(regType)) {
        var reg: DenseVector[Double] = DenseVector.ones[Double](mainVarLen) * regWeight
        reg = DenseVector.vertcat(reg, DenseVector.zeros(obj.length - reg.length))
        obj = obj + reg
        println("L1 is used for the LP DML problem")
      }
      println("Variable number:" + const.cols + ", Constrain number:" + const.rows)
      //构造目标函数
      val f: LinearProgram = new LinearProgram(obj.toArray)
      //val f = new LinearObjectiveFunction(obj.toArray, 0)
      val constraints = new util.ArrayList[LinearConstraint]
      val rows = const.rows
      for (i <- 0 until rows) {
        //获得约束矩阵的第i行
        val row: DenseVector[Double] = const(i, ::).t
        //添加约束（小于等于）
        f.addConstraint(new LinearBiggerThanEqualsConstraint(row.toArray, b(i), "c" + i))
      }
      f.setLowerbound(DenseVector.zeros[Double](obj.length).toArray)
      f.setMinProblem(true)
      //var solver:LinearProgramSolver = null
      var value:Array[Double] = null

      this.synchronized {
        //solver = SolverFactory.getSolver("LPSOLVE")
        value = SCPLPSolver.solver.solve(f)
      }

      val min = 0.0
      logger.info("Minimum value: " + min + "\n")
      //print decision variables
      logger.info("Solution:\n" + value.mkString("[", ",", "]"))
      (min, DenseVector(value))
    }
    else {
      logger.error("The length of the obj is not equal to the columns of constraints or the rows of constraints are not equal to the length of b.")
      (0, DenseVector.ones[Double](obj.length))
    }
  }

}
object SCPLPSolver{
  val solver:LinearProgramSolver = SolverFactory.getSolver("LPSOLVE")
}