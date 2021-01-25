package com.reremouse.dml.solver.optimizer

import breeze.linalg.DenseVector

class ArmijoSearch {

  val minStep = 1.0E-6


  def search(xk: DenseVector[Double], df: MathFunction, d: DenseVector[Double], args: Array[Any] = Array[Any]()): Double = {
    val rho = 0.1
    //var initStep: Double = 500
    val maxStep = 1000
    var x: DenseVector[Double] = null
    val c1 = 1.0E-4

    var step = 0
    var lambda: Double = 0
    var fp: Double = 0
    var fx: Double = 0
    do {
      lambda = Math.pow(rho, step)
      x = xk + lambda * d
      fp = df.computeObjective(x, args)
      fx = df.computeObjective(xk, args) + c1 * lambda * (d.t * df.computeGradient(xk, args))
      step += 1
    } while (step <= maxStep && fx < fp)
    //println("Found best lambda:"+lambda)
    if (lambda < minStep) lambda = minStep
    lambda
  }


}
