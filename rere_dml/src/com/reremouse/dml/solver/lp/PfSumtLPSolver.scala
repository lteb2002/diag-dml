package com.reremouse.dml.solver.lp

import breeze.linalg._
import breeze.numerics.{abs, log, pow}
import breeze.optimize.{DiffFunction, LBFGS}
import com.reremouse.dml.solver.optimizer.{ConstrainedMathFunction, DFP, Momentum, MultiplierMethod, RoundResult, SteepestDescent}
import com.reremouse.util.RereLogger

/**
  * Created by Lteb2002 on 2018-01-19.
  * Solve linear programming and its regularization problems with Barrier function methods
  * 该方法要求线性规划的约束条件全部为“>=”的不等式约束
  * Just for test purpose
  * Although this solver can solve small optimization problems correctly, it turned out that the underlying solver L-BFGS in Breeze.jar can not
  * solver big problems containing too many variables (more than 1000?). So this solver is substituted by its PfSumtLPSolverJulia counterpart, which invokes the Julia implementation
  * of the Penalty Method.
  */
@Deprecated
class PfSumtLPSolver extends RereLPSolver {
  val logger = RereLogger.getLogger(this.getClass)

  //Sumt方法中的u系数，迭代中逐渐变小
  var u: Double = 1.0

  //正则项权重
  //val regWeight: Double = 10

  private def buildInitX(totalV: Int, mainVar: Int): DenseVector[Double] = {
    var init: DenseVector[Double] = null
    if (mainVar != 0) {
      val p1 = DenseVector.rand[Double](mainVar) * 1.0E-2
      val p2 = DenseVector.ones[Double](totalV - mainVar) * 1.0E2
      init = DenseVector.vertcat(p1, p2)
      init = DenseVector.rand[Double](totalV) * 1.0E-2
//      init = DenseVector.ones(totalV)
    } else {
      init = DenseVector.ones(totalV)
    }
    init
  }

  /**
    * 求解线性规划问题（最小化目标函数，大于等于约束）
    *
    * @param obj   目标函数系统
    * @param const 约束矩阵
    * @param b     约束值
    * @return 取得线性规划的解
    */
  def solve(obj: DenseVector[Double],
            const: DenseMatrix[Double],
            b: DenseVector[Double],
            regType: String = "L2",
            regWeight: Double,
            mainVarLen: Int = 0): (Double, DenseVector[Double]) = {
    if (obj.length == const.cols && const.rows == b.length) {
      val ree = LPCoordinator.transformBiggerEqualsThanAsEquals(const, obj)
      val A = ree._1
      val c = ree._2
      println("BF LP SOLVER, Variable number:" + A.cols + ", Constrain number:" + A.rows)
      //val A=DenseMatrix.horzcat(const,-DenseMatrix.eye[Double](const.rows))
      //val c:DenseVector[Double]=DenseVector.vertcat(obj,DenseVector.zeros[Double](A.rows))
      var x0 = this.buildInitX(c.length, mainVarLen)
      println("Init x0: " + x0)

      val df = new ConstrainedMathFunction() {

        private def indicator(x: DenseVector[Double]): DenseVector[Double] = {
          val y = x.map(x => {
            if (x < 0) x * x else 0
          })
          return y
        }

        private def indicator2(x: DenseVector[Double]): DenseVector[Double] = {
          val y = x.map(x => {
            if (x < 0) 2 * x else 0
          })
          return y
        }

        private def bf(x: DenseVector[Double]): Double = {
          //          val y = sum(indicator(x)) + (A * x - b).t * (A * x - b)
          val y = sum(indicator(x)) + sum(pow((A * x - b), 2.0))
          return y
        }


        /**
          * 计算目标函数
          * compute objective function
          *
          * @param x
          * @param args
          * @return
          */
        override def computeObjective(x: DenseVector[Double], args: Array[Any]): Double = {
          var reg: Double = 0.0
          if (regType.toLowerCase().equals("l1")) {
            reg = sum(abs(x))
          } else if (regType.toLowerCase().equals("l2")) {
            reg = sum(pow(x, 2.0))
          }
          c.t * x + u * bf(x) + regWeight * reg
        }

        /**
          * 计算梯度
          * compute the gradient
          *
          * @param xx
          * @param args
          * @return
          */
        override def computeGradient(xx: DenseVector[Double], args: Array[Any]): DenseVector[Double] = {
          var reg: DenseVector[Double] = DenseVector.zeros[Double](xx.length)
          if (regType.toLowerCase().equals("l1")) {
            for (i <- 0 until xx.length) {
              reg(i) = if (xx(i) > 0) 1.0 else -1.0
            }
            reg = abs(DenseVector.ones[Double](xx.length))
          } else if (regType.toLowerCase().equals("l2")) {
            reg = xx * 2.0
          }
          val df1 = c + u * indicator2(xx) + u * 2 * A.t * (A * xx - b) + reg * regWeight
          //val df1 = c + br + reg * regWeight
          df1
        }

        /**
          * 障碍函数的值，用于求解SUMT迭代的停止条件
          * compute the value of the barrier function
          *
          * @param xx
          * @return
          */
        override def computeConstrainError(xx: DenseVector[Double]): DenseVector[Double] = {
          val v = DenseVector[Double](bf(xx))
          v
        }
      }
      val maxStep = 1.0E2 // max SUMT steps
      val increaseRate = 1.0E2 //the rate to increase punishment coefficient in SUMT steps
      var error = 0.0 // record error of constraint at current step
      var step = 0 // record current step
      val start = System.currentTimeMillis()
      var errorGap = 0.0 // error gap between steps
      var zeroGapTimes = 0 //record zero gap times, such that the SUMT steps can be stopped if no optimization is achieved for too many steps
      val allowedZeroGapTimes = 10

      val dfp = new DiffFunction[DenseVector[Double]] {
        override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
          (df.computeObjective(x, Array[Any]()), df.computeGradient(x, Array[Any]()))
        }
      }
      val lbfgs = new LBFGS[DenseVector[Double]](maxIter = 1.0E4.toInt, m = 700, tolerance = 1.0E-4)
      do {

        //        error = sum(abs(df.computeConstrainError(x0)))
        //println("Init step:" + step + ",error:" + error + ",u:" + u)
        //println("Objective value: " + df.computeObjective(x0, Array[Any]()))
        //print decision variables
        //println("Init X:" + x0)
        //println("---------------")

        //                        val m = new SteepestDescent(ifFixedStep = false, ifPositiveValue = false, precision = 1.0E-4)
        //                        x0 = m.gredientDescent(x0, df, Array[Any]())


        //以DFP方法求解
        //                                val m = new DFP(ifFixedStep = true, ifPositiveValue = false, precision = 1.0E-4)
        //                                x0 = m.gredientDescent(x0, df, Array[Any]())

        //                        val m = new Momentum(ifFixedStep = true, ifPositiveValue = true, precision = 1.0E-4)
        //                        x0 = m.gredientDescent(x0, df, Array[Any]())


        // m is the memory. anywhere between 3 and 7 is fine. The larger m, the more memory is needed.
        try {

          val newX0 = lbfgs.minimize(dfp, x0)
          errorGap = sum(pow((newX0 - x0), 2))
          x0=newX0
//          println("x0: " + x0)
        } catch {
          case e: Exception => {
            println(x0)
            e.printStackTrace()
            throw e
          }
        }
        val newError = sum(abs(df.computeConstrainError(x0)))
//        errorGap = error - newError
        if (abs(errorGap) < 1.0E-32) {
          zeroGapTimes += 1
        } else { //如果有一次gap不为0,0记数清零 eliminate zero_gap counting once the gap is not zero
          zeroGapTimes = 0
        }
        error = newError

        println("SUMT step:" + step + ",error:" + error + ",u:" + u + ",errorGap:" + errorGap)
        //println("Minimum value: " + df.computeObjective(x0, Array[Any]()))
        //print decision variables
        //println("Solution:" + x0)
        //println("---------------")
        u = u * increaseRate
        step += 1

      } while (error > 1.0E-16 && step < maxStep && zeroGapTimes < allowedZeroGapTimes)
      var ans = x0
      var min: Double = df.computeObjective(x0, Array[Any]())
      val end = System.currentTimeMillis()
      println("The answer:" + ans)
      println("Time cost:" + (end - start) / 1000.0 + "s")
      (RoundResult(min), RoundResult(ans))
    } else {
      logger.error("The length of the obj is not equal to the columns of constraints or the rows of constraints are not equal to the length of b.")
      (0, DenseVector.ones[Double](obj.length))
    }
  }

}
