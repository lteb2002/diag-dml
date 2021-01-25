package com.reremouse.dml.solver.optimizer.admm

import breeze.linalg.DenseVector

trait ConstrainedErrorFunction {

  def calculate(x1:DenseVector[Double],x2:DenseVector[Double]):Double

}
