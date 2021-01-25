package com.reremouse.dml.solver.optimizer.admm.lp

import breeze.linalg.{DenseMatrix, DenseVector, inv, pinv}
import com.reremouse.dml.solver.optimizer.admm.AdmmUpdater
import scpsolver.constraints.LinearEqualsConstraint
import scpsolver.lpsolver.SolverFactory
import scpsolver.problems.LinearProgram

class X2UpdaterLP extends AdmmUpdater {

  var c1: DenseVector[Double] = null
  var c2: DenseVector[Double] = null
  var A1: DenseMatrix[Double] = null
  var A2: DenseMatrix[Double] = null
  var b: DenseVector[Double] = null

  var u:Double = 100

  var A2A1: DenseMatrix[Double] = null
  var A2TA2_1: DenseMatrix[Double] = null
  var A2TA2: DenseMatrix[Double] = null
  var A2TA1: DenseMatrix[Double] = null
  var A2t: DenseMatrix[Double] = null

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
    A2TA2 = A2.t * A2
    try {
      A2TA2_1 = inv(A2TA2)
    } catch {
      case ex: Exception => {
        println("pinv")
        A2TA2_1 = pinv(A2TA2)
      }
    }
    A2TA1 = A2.t * A1
    A2t=A2.t
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
                      beta: Double=2,name:String="X2 updater",
                      regType: String = "l2",
                      regWeight: Double=1): DenseVector[Double] = {
    val newC= c2.t- (lambda.t*A2)
    val newA = A2
    val newB: DenseVector[Double]=b-(A1*x1)
    val lp = new LinearProgram(newC.t.toArray)
    val rows=newA.rows
    for(i<- 0 until rows){
      val row=newA(i,::).t.toArray
      lp.addConstraint(new LinearEqualsConstraint(row, newB(i), "a"+i))
    }
    lp.setMinProblem(true)
    lp.setLowerbound(DenseVector.zeros[Double](c2.length).toArray)
    val solver = SolverFactory.newDefault
    val sol = solver.solve(lp)
    val xx=DenseVector[Double](sol)
    println(name+":"+xx)
    xx
  }

}
