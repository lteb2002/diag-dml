package com.reremouse.dml.solver.optimizer.admm

import breeze.linalg.DenseVector

/**
  *
  */
trait AdmmUpdater {


  /**
    * 输入参数，输出更新后的值
    * @param partA
    * @param partB
    * @param lambda
    * @param beta
    * @return
    
    */
  def update(partA:DenseVector[Double],
             partB:DenseVector[Double],
             lambda:DenseVector[Double],
             beta:Double=0.5,
             name:String="updater",
             regType: String = "l2",
             regWeight: Double=1
            ):DenseVector[Double]


}
