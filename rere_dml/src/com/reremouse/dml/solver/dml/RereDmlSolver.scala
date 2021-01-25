package com.reremouse.dml.solver.dml

import breeze.linalg.DenseMatrix
import com.reremouse.dml.model.Triplelet

/**
  * Created by RereMouse on 2018-01-19.
  */
trait RereDmlSolver {

  /**
    * 求解度量学习问题
    *
    * @param trips 三元组
    * @return 返回投影矩阵
    */
  def computeProjectionMatrix(trips: Seq[Triplelet],
                              lpSolver:String,
                              regType: String = "L1",
                              regWeight: Double): DenseMatrix[Double]


}

object RereDmlSolver {

  /**
    * 工厂方法，返回一个度量学习求解器
    * 可能是线性规划方法或者半定规划方法
    *
    * @return
    */
  def createSolver(): RereDmlSolver = {
    //val solver = new RereDmlSDPSolver()
    val solver = new RereDmlLpSolver()
    return solver
  }
}
