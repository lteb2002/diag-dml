package com.reremouse.dml.solver.optimizer

import breeze.linalg.DenseVector

trait MathFunction {


  def computeObjective(x:DenseVector[Double],args:Array[Any]=Array[Any]()):Double

  /**
    * 计算梯度
    * @param x
    * @return
    */
  def computeGradient(x:DenseVector[Double],args:Array[Any]=Array[Any]()):DenseVector[Double]

}
