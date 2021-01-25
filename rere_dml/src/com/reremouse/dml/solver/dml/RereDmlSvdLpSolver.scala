package com.reremouse.dml.solver.dml

import breeze.linalg._
import com.reremouse.dml.model.{RereData, Triplelet}

/**
  * Created by RereMouse on 2018-01-18.
  */
class RereDmlSvdLpSolver extends RereDmlLpSolver {


  /**
    * 用线性规划方法求解度量学习问题
    * min 三元组所有同类点的距离之和 + u*松弛变量和
    * subject to 三元组中每一个： 异类距离 - 同类距离 >= 1- 松弛变量
    *
    * @param trips 三元组 集
    * @return
    */
  override def computeProjectionMatrix(trips: Seq[Triplelet],lpSolver:String,regType: String , regWeight: Double): DenseMatrix[Double] = {


    val m=super.computeProjectionMatrix(trips,lpSolver,regType,regWeight)
    m
  }



}
