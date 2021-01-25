package com.reremouse.dml.solver.optimizer.test

import breeze.linalg.{DenseVector, norm}
import breeze.optimize.{DiffFunction, LBFGS}
import com.reremouse.dml.solver.optimizer.{DFP, MathFunction}

object LbfgsTest {

  def main(args: Array[String]): Unit = {

    val df = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val xx: DenseVector[Double] = x + 1.0d
        val dfx = (x * (2.0d)) + 2.0d
        (xx.t * xx,dfx)
      }
    }

    val curX = DenseVector(6.0, 6.0)
    val precision = 0.00001
    val previousStepSize = 1 / precision
    val lbfgs = new LBFGS[DenseVector[Double]](maxIter=100, m=3)
    // m is the memory. anywhere between 3 and 7 is fine. The larger m, the more memory is needed.
    val optimum = lbfgs.minimize(df,curX)
    println(optimum)

  }


}
