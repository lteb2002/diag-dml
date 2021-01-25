package com.reremouse.dml.solver.optimizer.admm

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}
import breeze.numerics.{abs, log, sqrt}
import breeze.optimize.{DiffFunction, LBFGS}
import com.reremouse.dml.solver.optimizer.test.AdmmLpSolverBfSumtTest
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MathFunction, MultiplierMethod, SteepestDescent}

class SumtMethodTest {

  var lambda = DenseVector.ones[Double](3) + 0.5


  def iterate(v: Double, x: DenseVector[Double], df: MathFunction) = {}


  def solve(initX: DenseVector[Double],
            df: ConstrainedMathFunction,
            args: Array[Any] = Array[Any]()
           ): (DenseVector[Double], Double, Double) = {
    var error = 1.0
    var newX: DenseVector[Double] = initX
    var step = 0
    val maxStep = 100000000
    val start = System.currentTimeMillis()
    do {
      val args0 = Array[Any](lambda)
      //      val method = new SteepestDescent(stepSize = 0.001,
      //        ifFixedStep = true,
      //        ifPositiveValue = true,
      //        precision = 1.0E-3) //ADMM的子迭代中，可以取较低精度，这会造成更多的ADMM迭代，但每一步时间更快，总时间减少
      //      newX = method.gredientDescent(newX, df)
      val dfp = new DiffFunction[DenseVector[Double]] {
        override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
          (df.computeObjective(x, args = args0), df.computeGradient(x, args = args0))
        }
      }
      val lbfgs = new LBFGS[DenseVector[Double]](maxIter = 100.toInt, m = 7, tolerance = 1.0E-1)
      // m is the memory. anywhere between 3 and 7 is fine. The larger m, the more memory is needed.
      newX = lbfgs.minimize(dfp, newX)
      lambda=lambda - 1/SumtMethodTest.u*(df.computeConstrainError(newX))
      error = abs(-sum(SumtMethodTest.u * log(newX)))
      println("SUMT step:" + step + ",error:" + error + ",u:" + SumtMethodTest.u+", lambda:"+lambda)

      SumtMethodTest.u = SumtMethodTest.u * 0.8
      //println(SumtMethodTest.u > 1.0E-10)
      step += 1
    } while (error > 1.0E-5 && step < maxStep && SumtMethodTest.u > 1.0E-100)

    println("result,x:" + newX)
    val end = System.currentTimeMillis()
    println("Time cost:" + (end - start) / 1000.0 + "s")
    val args0 = Array[Any](lambda)
    (newX, df.computeObjective(newX,args0), error)
  }

}

object SumtMethodTest {
  var u: Double = 1


  def main(args: Array[String]): Unit = {

    val x0 = DenseVector[Double](1.0, 1.0, 1.0, 5, sqrt(15.0), sqrt(29.0))

    //val x0 = DenseVector.ones[Double](6)


    val beta: Double = 2.0

    val c = DenseVector[Double](-3, -1, -2, 0, 0, 0)
    val b = DenseVector[Double](30, 24, 36)

    val A = DenseMatrix(
      (1.0, 1.0, 3.0, 1.0, 0.0, 0.0),
      (2.0, 2.0, 5.0, 0.0, 1.0, 0.0),
      (4.0, 1.0, 2.0, 0.0, 0.0, 1.0))

    val df = new ConstrainedMathFunction() {
      override def computeObjective(x: DenseVector[Double], args: Array[Any] = Array[Any]()): Double = {
        val newL: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
        c.t * x - sum(u * log(x)) - newL.t * (A * x - b)
        //c1.t*(x*:*x) +c2.t * (x2*:*x2)  - lambda.t * (A1*(x*:*x)+A2*(x2*:*x2)-b) + (beta/2)*norm(A1*(x*:*x)+A2*(x2*:*x2)-b)
      }

      override def computeGradient(x: DenseVector[Double], args: Array[Any] = Array[Any]()): DenseVector[Double] = {
        val newL: DenseVector[Double] = args(0).asInstanceOf[DenseVector[Double]]
        //println("uu:"+uu)
        val dfx = c - u / x - A.t * newL
        //val dfx = 2.0*(c1*:*x)  - (2.0*A1.t * lambda)*:*x + (2.0*beta*A1.t *(A1*(x*:*x) + A2*(x2*:*x2) - b)*:*x)
        dfx
      }
      override def computeConstrainError(xx: DenseVector[Double]): DenseVector[Double] = {
        A * (xx ) - b
      }
    }
    val start = System.currentTimeMillis()
    val method = new SumtMethodTest()
    val re = method.solve(x0, df)
    val ans = re._1
    val obj: Double = re._2
    print((ans).toString() + ",obj:")
    println(obj)
    val end = System.currentTimeMillis()
    println("Time cost:" + (end - start) / 1000.0 + "s")
  }


}
