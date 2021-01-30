package com.reremouse.dml.solver.lp

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * 解线性规划求解器接口，本应用中可能有多种线性规划求解器
  */
trait RereLPSolver {

  /**
    * 求解线性规划问题（最小化目标函数，小于等于约束）
    *
    * @param obj   目标函数系统
    * @param const 约束矩阵
    * @param b     约束值
    * @param regType     正则化类型，默认无
    * @return 取得线性规划的解
    */
  def solve(obj: DenseVector[Double],
            const: DenseMatrix[Double],
            b: DenseVector[Double],
            regType:String="none",
            regWeight:Double,
            mainVarLen:Int=0): (Double, DenseVector[Double])


}

object RereLPSolver {
  /**
    * 创建一个线性规划求解器（工厂方法）
    *
    * @return 线性规划求解器
    */
  def createLPSolver(mainVnum: Int , s1Vnum: Int , s2Vnum: Int ,method:String="simplex"): RereLPSolver = {
    val solver: RereLPSolver  = method.toLowerCase() match {
      case "simplex"=> new SCPLPSolver
      //since Java sucks at numerical computing, the Julia implementation of BfSumtLPSolver is recommended
//      case "bf"=> new BfSumtLPSolver
      case "pf"=> new PfSumtLPSolverJulia()
      case "admm"=> new AdmmLPSolverJulia()
      case _=> new SCPLPSolver
    }
    return solver
  }
}
