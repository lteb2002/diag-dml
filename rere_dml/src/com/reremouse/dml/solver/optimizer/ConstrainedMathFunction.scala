package com.reremouse.dml.solver.optimizer

import breeze.linalg.DenseVector

trait ConstrainedMathFunction extends MathFunction{

  def computeConstrainError(curX:DenseVector[Double]):DenseVector[Double]

}
