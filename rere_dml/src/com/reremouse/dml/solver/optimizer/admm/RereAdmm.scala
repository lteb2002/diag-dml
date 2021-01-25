package com.reremouse.dml.solver.optimizer.admm

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import com.reremouse.dml.solver.optimizer.RoundResult

/**
  * ADMM求解器
  */
class RereAdmm {

  var x1: DenseVector[Double] = null
  var x2: DenseVector[Double] = null

  var c1: DenseVector[Double] = null
  var c2: DenseVector[Double] = null
  var A1: DenseMatrix[Double] = null
  var A2: DenseMatrix[Double] = null
  var ATB: DenseMatrix[Double] = null
  var b: DenseVector[Double] = null

  var lambda: DenseVector[Double] = null
  var beta: Double = 1

  var x1Updater: AdmmUpdater = null
  var x2Updater: AdmmUpdater = null
  var lambdaUpdater: AdmmUpdater = null
  var cef: ConstrainedErrorFunction = null
  var tolerance=1.0E-4
  var regType: String = "l2"
  var regWeight: Double=1

  def this(x10: DenseVector[Double],
           x20: DenseVector[Double],
           c10: DenseVector[Double],
           c20: DenseVector[Double],
           A10: DenseMatrix[Double],
           A20: DenseMatrix[Double],
           b0: DenseVector[Double],
           lambda0: DenseVector[Double],
           beta0: Double = 0.02,
           x1Updater0: AdmmUpdater,
           x2Updater0: AdmmUpdater,
           lambdaUpdater0: AdmmUpdater,
           constrainedErrorFunction: ConstrainedErrorFunction,
           tolerance0: Double=1.0E-4,
           regType0: String = "l2",
           regWeight0: Double=1
          ) {
    this()
    this.x1 = x10
    this.x2 = x20
    this.c1 = c10
    this.c2 = c20
    this.A1 = A10
    this.A2 = A20
    this.b = b0
    this.lambda = lambda0
    this.beta = beta0
    this.x1Updater = x1Updater0
    this.x2Updater = x2Updater0
    this.lambdaUpdater = lambdaUpdater0
    this.cef = constrainedErrorFunction
    this.ATB = A1.t * A2
    this.tolerance=tolerance0
    this.regType=regType0
    this.regWeight=regWeight0
  }

  private def updateX1(x2k: DenseVector[Double],
                       lambdak: DenseVector[Double],
                       x1Updater: AdmmUpdater): DenseVector[Double] = {
    val x1kp = x1Updater.update(x1, x2k, lambdak, beta, "X1 updater",regType,regWeight)
    val error = x1kp - x1
    x1 = x1kp
    error
  }

  private def updateX2(x1kp: DenseVector[Double],
                       lambdak: DenseVector[Double],
                       x2Updater: AdmmUpdater): DenseVector[Double] = {
    val x2kp = x2Updater.update(x1kp, x2, lambdak, beta, "X2 updater",regType,regWeight)
    val error = x2kp - x2
    x2 = x2kp
    error
  }

  private def updateLambda(x1kp: DenseVector[Double],
                           x2kp: DenseVector[Double],
                           lambdaUpdater: AdmmUpdater): DenseVector[Double] = {
    val lkp = lambdaUpdater.update(x1kp, x2kp, lambda, beta, "Lambda updater")
    val error = lkp - lambda
    lambda = lkp
    error
  }

  def iterate(newBeta: Double): (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]) = {
    this.beta = newBeta
    val e1 = this.updateX1(this.x2, this.lambda, this.x1Updater)
    val e2 = this.updateX2(this.x1, this.lambda, this.x2Updater)
    val e3 = this.updateLambda(this.x1, this.x2, lambdaUpdater)
    (x1, x2, lambda, e1, e2)
  }

  def solve(): (DenseVector[Double], DenseVector[Double], Double, Double) = {
    var p1: DenseVector[Double] = null
    var p2: DenseVector[Double] = null
    var obj: Double = 0
    var rkp = 1.0
    var skp: Double = 1.0
    var error=0.0
    var step = 0
    val maxStep = 1.0E4
    //var constrainError:DenseVector[Double]=DenseVector.ones[Double](b.length)
    do {
      step += 1
      val re = iterate(beta)
      p1 = re._1
      p2 = re._2
      error=norm(re._4)+norm(re._5)
      val gap = re._5
      skp = norm(beta * ATB * gap)//对偶问题残差
      rkp = cef.calculate(p1, p2)//原问题残差
      val u = 10.0
      val p = 2.0
      if (rkp > u * skp&& skp!=0) {
        beta = beta * p
      } else if (skp > u * rkp && rkp!=0) {
        beta = beta / p
      }
      //val newError = A1 * (p1*:*p1) + A2 * (p2*:*p2) - b
      //val newError=cef.calculate(p1,p2 )
      //if (norm(newError) / norm(constrainError) >= 0.8) beta = beta * 0.5
      //error2 = norm(newError)
      //println("ADMM STEP:" + step + ",error1:" + rkp+",error2:"+skp+",error3:"+error+",beta:"+beta)
    } while ((rkp > tolerance || skp > tolerance || error>tolerance) && step < maxStep)
    //obj = c1.t * (p1*:*p1) + c2.t * (p2*:*p2)
    println("ADMM ONE TURN result,p1:" + p1 + ",p2:" + p2)
    (p1, p2, obj, rkp)
  }


}
