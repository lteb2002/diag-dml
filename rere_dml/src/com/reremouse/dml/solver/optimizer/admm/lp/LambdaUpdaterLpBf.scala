package com.reremouse.dml.solver.optimizer.admm.lp

import breeze.linalg.{DenseMatrix, DenseVector}
import com.reremouse.dml.solver.optimizer.admm.AdmmUpdater

class LambdaUpdaterLpBf extends AdmmUpdater {


  var A1: DenseMatrix[Double] = null
  var A2: DenseMatrix[Double] = null
  var b: DenseVector[Double] = null


  def this(A10: DenseMatrix[Double], A20: DenseMatrix[Double], b0: DenseVector[Double]) {
    this()
    this.A1 = A10
    this.A2 = A20
    this.b = b0
  }


  /**
    * 输入参数，输出更新后的值
    *
    * @param partA
    * @param partB
    * @param lambda
    * @param beta
    * @return
    *
    */
  override def update(partA: DenseVector[Double],
                      partB: DenseVector[Double],
                      lambda: DenseVector[Double],
                      beta: Double=.5,name:String="Lambda updater",
                      regType: String = "l2",
                      regWeight: Double=1): DenseVector[Double] = {
    val xl = lambda - beta * (A1*(partA)+A2*(partB)-b)
    //println(name+":"+xl)
    xl
  }
}
