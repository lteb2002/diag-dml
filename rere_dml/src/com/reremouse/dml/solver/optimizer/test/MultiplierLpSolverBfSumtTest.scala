package com.reremouse.dml.solver.optimizer.test

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import breeze.numerics.log
import com.reremouse.dml.solver.lp.{PfSumtLPSolver, MultiplierLPSolverBfSumt}
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, MultiplierMethod, RoundResult}

//测试
/**
  * 测试基于乘子法求解线性规划问题
  */
object MultiplierLpSolverBfSumtTest {


  def main(args: Array[String]): Unit = {

    //val x0 = DenseVector.ones[Double](6)

    val lambda = DenseVector.ones[Double](3) + 0.5
    val beta: Double = 5.0

    val c = DenseVector[Double](-3, -1, -2)
    val b = DenseVector[Double](30, 24, 36)

    val A = DenseMatrix(
      (1.0, 1.0, 3.0),
      (2.0, 2.0, 5.0),
      (4.0, 1.0, 2.0))


    val start = System.currentTimeMillis()
    val method = new MultiplierLPSolverBfSumt()
    //val method = new BfSumtLPSolver()
    val re=method.solve(c,A,b,"L2",0)
    val ans = re._2
    val obj:Double=re._1
    print(RoundResult(ans).toString()+",obj:")
    println(RoundResult(obj))
    val end = System.currentTimeMillis()
    println("Time cost:"+(end-start)/1000.0+"s")
  }

}
