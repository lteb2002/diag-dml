package com.reremouse.dml.solver.optimizer.admm

import breeze.linalg.{DenseMatrix, DenseVector, norm, sum}
import breeze.numerics.{abs, log, sqrt}
import breeze.optimize.{DiffFunction, LBFGS}
import com.reremouse.dml.solver.optimizer.test.AdmmLpSolverBfSumtTest
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MathFunction, MultiplierMethod, SteepestDescent}

class SumtMethod {


  def iterate(v: Double, x: DenseVector[Double], df: MathFunction) = {}


  def solve(initX: DenseVector[Double],
            df: MathFunction,
            args: Array[Any] = Array[Any]()
           ): (DenseVector[Double],Double,Double) = {
    var error = 1.0
    var newX: DenseVector[Double] = initX
    var step = 0
    val maxStep = 100000000
    val start = System.currentTimeMillis()
    do {
      val method = new SteepestDescent(stepSize = 0.001,
        ifFixedStep = true,
        ifPositiveValue = true,
        precision = 1.0E-3) //ADMM的子迭代中，可以取较低精度，这会造成更多的ADMM迭代，但每一步时间更快，总时间减少
      newX = method.gredientDescent(newX, df)
      //      val dfp = new DiffFunction[DenseVector[Double]] {
      //        override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
      //          (df.computeObjective(x), df.computeGradient(x))
      //        }
      //      }
      //      val lbfgs = new LBFGS[DenseVector[Double]](maxIter = 100.toInt, m = 7, tolerance = 1.0E-1)
      //      // m is the memory. anywhere between 3 and 7 is fine. The larger m, the more memory is needed.
      //      newX = lbfgs.minimize(dfp, newX)
      error = abs(-sum(AdmmLpSolverBfSumtTest.u * log(newX)))
      println("SUMT step:" + step + ",error:" + error + ",u:" + SumtMethod.u)
      SumtMethod.u = SumtMethod.u * 0.3
      step += 1
    } while (error > 2.0E-3 && step < maxStep)

    println("result,x:" + newX)
    val end = System.currentTimeMillis()
    println("Time cost:" + (end - start) / 1000.0 + "s")
    (newX,df.computeObjective(newX),error)
  }

}

object SumtMethod {
  var u: Double = 1.0



  def main(args: Array[String]): Unit = {

    val x0 = DenseVector[Double](1.0, 1.0, 1.0, 5, sqrt(15.0), sqrt(29.0))

    //val x0 = DenseVector.ones[Double](6)

    val lambda = DenseVector.ones[Double](3) + 0.5
    val beta: Double = 2.0

    val c = DenseVector[Double](-3, -1, -2, 0, 0, 0)
    val b = DenseVector[Double](30, 24, 36)

    val A = DenseMatrix(
      (1.0, 1.0, 3.0, 1.0, 0.0, 0.0),
      (2.0, 2.0, 5.0, 0.0, 1.0, 0.0),
      (4.0, 1.0, 2.0, 0.0, 0.0, 1.0))

    val df = new MathFunction() {
      override def computeObjective(x: DenseVector[Double],args:Array[Any]=Array[Any]()): Double = {
        c.t*x- sum(AdmmLpSolverBfSumtTest.u*log(x))- sum(u*log(x)) - lambda.t * (A*x-b) + (beta/2)*(Math.pow(norm(A*x-b),2))
        //c1.t*(x*:*x) +c2.t * (x2*:*x2)  - lambda.t * (A1*(x*:*x)+A2*(x2*:*x2)-b) + (beta/2)*norm(A1*(x*:*x)+A2*(x2*:*x2)-b)
      }
      override def computeGradient(x: DenseVector[Double],args:Array[Any]=Array[Any]()): DenseVector[Double] = {
        //println("uu:"+uu)
        val dfx = c - u/x -A.t*lambda
        //val dfx = 2.0*(c1*:*x)  - (2.0*A1.t * lambda)*:*x + (2.0*beta*A1.t *(A1*(x*:*x) + A2*(x2*:*x2) - b)*:*x)
        dfx
      }
    }
    val start = System.currentTimeMillis()
    val method = new SumtMethod()
    val re=method.solve(x0, df)
    val ans = re._1
    val obj:Double=re._2
    print((ans).toString()+",obj:")
    println(obj)
    val end = System.currentTimeMillis()
    println("Time cost:"+(end-start)/1000.0+"s")
  }



}
