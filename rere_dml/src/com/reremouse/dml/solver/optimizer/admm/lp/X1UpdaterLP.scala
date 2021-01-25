package com.reremouse.dml.solver.optimizer.admm.lp

import breeze.linalg.{DenseMatrix, DenseVector, inv, pinv}
import com.reremouse.dml.solver.optimizer.admm.AdmmUpdater
import scpsolver.constraints.LinearEqualsConstraint
import scpsolver.lpsolver.SolverFactory
import scpsolver.problems.LinearProgram

class X1UpdaterLP extends AdmmUpdater {

  var c1: DenseVector[Double] = null
  var c2: DenseVector[Double] = null
  var A1: DenseMatrix[Double] = null
  var A2: DenseMatrix[Double] = null
  var b: DenseVector[Double] = null

  var u:Double = 100

  var A1TA1: DenseMatrix[Double] = null
  var A1TA1_1: DenseMatrix[Double] = null
  var A1TA2: DenseMatrix[Double] = null
  var A1t: DenseMatrix[Double] = null

  def this(c10: DenseVector[Double],
           c20: DenseVector[Double],
           A10: DenseMatrix[Double],
           A20: DenseMatrix[Double],
           b0: DenseVector[Double]) {
    this()
    this.c1 = c10
    this.c2 = c20
    this.A1 = A10
    this.A2 = A20
    this.b = b0
    A1TA1 = A1.t * A1
    try {
      A1TA1_1 = inv(A1TA1)
    } catch {
      case ex: Exception => {
        println("pinv")
        A1TA1_1 = pinv(A1TA1)
      }
    }
    A1TA2 = A1.t * A2
    A1t=A1.t
  }


  /**
    * 输入参数，输出更新后的值
    *
    * @param x1
    * @param x2
    * @param lambda
    * @param beta
    * @return
    *
    */
  override def update(x1: DenseVector[Double],
                      x2: DenseVector[Double],
                      lambda: DenseVector[Double],
                      beta: Double=2,name:String="X1 updater",
                      regType: String = "l2",
                      regWeight: Double=1): DenseVector[Double] = {
    val newC: DenseVector[Double]= (c1.t- (lambda.t*A1)).t
    val newA: DenseMatrix[Double] = A1
    val newB: DenseVector[Double]=b-(A2*x2)
//    println("c:"+newC.data.mkString(","))
//    println("A:"+newA)
//    println("b:"+newB)
    val lp = new LinearProgram(newC.toArray)
    val rows=newA.rows
    for(i<- 0 until rows){
      val row=newA(i,::).t.toArray
      //println(row.mkString(","))
      lp.addConstraint(new LinearEqualsConstraint(row, newB(i), "a"+i))
    }
    lp.setMinProblem(true)
    val low=DenseVector.zeros[Double](c1.length).toArray
    //println(low.mkString(","))
    lp.setLowerbound(low)
    val solver = SolverFactory.newDefault
    val sol = solver.solve(lp)
    val xx=DenseVector[Double](sol)
    println(name+":"+xx)
    xx
  }

}
