package com.reremouse.dml.solver.optimizer

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.optimize.{DiffFunction, LBFGS}

class MultiplierMethod extends Serializable {


  val maxStep = 1E4

  //全局变量v，迭代中被更新
  var v: DenseVector[Double] = null
  //全局变量tho，迭代中被更新
  var tho: Double = 0.0

  val alpha: Double = 5
  //alpha>1
  val beta = 0.8 //0<beta<1

  var constrainError: DenseVector[Double] = null


  def this(initV: DenseVector[Double], initTho: Double) {
    this()
    this.v = initV
    this.tho = initTho
  }


  /**
    *
    * @param curX
    * @param df
    * @param method
    * @return
    */
  private def iterate(curX: DenseVector[Double],
                      df: ConstrainedMathFunction,
                      method: String = "lbfgs",
                      ifPositiveValue: Boolean): Tuple2[DenseVector[Double], Double] = {
    val args0 = Array[Any](v, tho)
    var newX = curX

    method.toLowerCase() match {
      case "momentum" => {
        //以最速下降法求解
        val m = new Momentum(ifFixedStep = false, ifPositiveValue =ifPositiveValue, precision = 1.0E-4)
        newX = m.gredientDescent(curX, df, args = args0)
      }
      case "steepest" => {
        //以最速下降法求解
        val m = new SteepestDescent(ifFixedStep = false, ifPositiveValue =ifPositiveValue, precision = 1.0E-4)
        newX = m.gredientDescent(curX, df, args = args0)
      }
      case "dfp" => {
        //以DFP方法求解
        val m = new DFP(ifFixedStep = false, ifPositiveValue =ifPositiveValue, precision = 1.0E-4)
        newX = m.gredientDescent(curX, df, args = args0)
      }
      case "lbfgs" => {
        //以LBFS方法求解
        val dfp = new DiffFunction[DenseVector[Double]] {
          override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
            (df.computeObjective(x, args = args0), df.computeGradient(x, args = args0))
          }
        }
        val lbfgs = new LBFGS[DenseVector[Double]](maxIter = 1.0E2.toInt, m = 20, tolerance = 1.0E-4)
        // m is the memory. anywhere between 3 and 7 is fine. The larger m, the more memory is needed.
        try {
          newX = lbfgs.minimize(dfp, curX)
          //将精度保持在小数点后8位
          //newX = RoundResult(newX, 8)
        } catch {
          case e: Exception => {
            println(newX)
            e.printStackTrace()
            throw e;
          }
        }
      }
      case _ => new {
        //以DFP方法求解
        val m = new DFP(ifFixedStep = false, ifPositiveValue =ifPositiveValue, precision = 1.0E-4)
        newX = m.gredientDescent(curX, df, args = args0)
      }
    }
    //保证所有元素均为正
    newX = newX.map((el: Double) => {
      var newV = el
      if (el < 0 && ifPositiveValue) {
        newV = 0
      }
      newV
    })

    val newError: DenseVector[Double] = df.computeConstrainError(newX)
    val normError = norm(newError)
    if (normError / norm(constrainError) >= beta) {
      if (tho < Int.MaxValue) {
        tho = alpha * tho
      }
      if (tho.isNaN) println("Tho now is too large to be a valid num")
    }
    constrainError = newError
    if (normError < Int.MaxValue) {
      v = v - tho * constrainError
    }
    (newX, normError)
  }


  /**
    *
    * @param curX x初始值
    * @param df
    * @param precision
    * @param previousStepSize
    * @return
    */
  def gredientDescent(curX: DenseVector[Double],
                      df: ConstrainedMathFunction,
                      precision: Double = 1.0E-4,
                      previousStepSize: Double = 10000000,
                      ifPositiveValue: Boolean = false): (DenseVector[Double], Double, Array[Any]) = {
    //println("Init x:"+curX)
    var (newX, stepSize) = (curX, previousStepSize)
    var step = 1
    this.constrainError = df.computeConstrainError(curX)
    try {
      do {
        val re = this.iterate(newX, df, ifPositiveValue = ifPositiveValue)
        newX = re._1
        stepSize = re._2
        step += 1
      } while (stepSize > precision && step <= maxStep)
      println("The local minimum is reached by MultiplierMethod at " + newX)
      println("Total steps:" + step)
      val args0 = Array[Any](v, tho)
      (newX, df.computeObjective(newX, args0), args0)
    } catch {
      case e: Exception =>
        e.printStackTrace()
        println(newX)
        val args0 = Array[Any](v, tho)
        (newX, df.computeObjective(newX, args0), args0)
    }

  }
}



