package com.reremouse.dml.solver.dml

import breeze.linalg._
import breeze.numerics.sqrt
import com.reremouse.dml.model.{DistanceMetricsSum, Triplelet}
import com.reremouse.dml.solver.lp.RereLPSolver
import com.reremouse.dml.solver.optimizer.RoundResult
import com.reremouse.dml.tool.DmlWeight
import com.reremouse.util.RereLogger

/**
  * 基于线性规划的DML求解器，求出的是一对角映射矩阵
  */
class RereDmlLpSolver extends RereDmlSolver {


  val maxConstraints: Int = 1.0E7.toInt
  val maxValue: Double = Double.MaxValue

  val u: Double = 10 //松弛变量和的系数


  val logger = RereLogger.getLogger(this.getClass)

  /**
    * 用线性规划方法求解度量学习问题
    * min 三元组所有同类点的距离之和 + u*松弛变量和
    * subject to 三元组中每一个： 异类距离 - 同类距离 >= 1- 松弛变量
    *
    * @param trips 三元组 集
    * @return
    */
  def computeProjectionMatrix(trips: Seq[Triplelet],
                              lpSolver: String,
                              regType: String = "L1",
                              regWeight0: Double): DenseMatrix[Double] = {
    //原变量数
    val dim = trips(0).getXj.getData.length
    //计算同类间距
    val dms = new DistanceMetricsSum(dim)
    for (trip <- trips) {
      val w = DmlWeight.computeWeight(trip)
      dms.addToADistance(trip.getXi.getData, trip.getXj.getData, w)
    }
    //三元组汇总形成的目标函数系数矩阵
    val cc = dms.exportMatrix.toDenseMatrix
    //构建MAP防止出现线束系数安全相同的情况
    var targetsMap = Map[String, DenseVector[Double]]()
    trips.foreach(t => {
      val ff = this.computeCoe(t.getXi.getData, t.getXj.getData, t.getXk.getData)
      //存放唯一的约束函数的系数前半部分
      targetsMap += (ff.toArray.mkString("-") -> ff)
    })
    logger.info("Trips size:" + trips.size + ", valid target constraits size:" + targetsMap.size)
    //临时存放算出的所有违约点的系数前半部分
    var targets = targetsMap.values.toSeq
    //允许的最大约束数
    if (targets.size > maxConstraints) {
      targets = scala.util.Random.shuffle(targets).take(maxConstraints)
    }
    //约束数
    val size = targets.size

    //原变量约束系数矩阵，按行向量数组构建
    val m1: DenseMatrix[Double] = DenseMatrix.zeros(size, dim)
    for (i <- 0 until size) {
      m1(i, ::) := targets(i).t
    }
    //约束矩阵中的松弛变量系数
    val m2 = DenseMatrix.eye[Double](size)
    //全约束矩阵
    var m = DenseMatrix.horzcat(m1, m2)
    //构建约束变量大于0的矩阵
    //变量大于0的处理将由求解算法解决
    //val m0 = DenseMatrix.eye[Double](dim + size)
    //m = DenseMatrix.vertcat(m, m0)
    val fnorm = sqrt(sum(m *:* m))
    println("F norm of A:" + fnorm)
    logger.info("Constrain matrix:\n" + m(::, 0 until dim))
    //原变量系数
    val obj1: DenseVector[Double] = diag(cc)
    //目标函数中的松弛变量系数
    val obj2: DenseVector[Double] = DenseVector.ones[Double](size) * u * fnorm
    //全目标函数系数
    val obj = DenseVector.vertcat(obj1, obj2)
    logger.info("obj:" + obj(0 until dim))

    //约束常量
    var b = DenseVector.ones[Double](size) * fnorm
    //为保证变量大于0而构建的约束变量下界
    //val b0 = DenseVector.zeros[Double](dim + size)
    //b = DenseVector.vertcat(b, b0)
    logger.info("b:" + b(0 until dim))
    //线性规划求解器
    val solverName = if ("auto".equals(lpSolver)) {
      //如果为自动选取求解器，则按以下规则
      regType.toLowerCase() match {
        case "none" => "simplex"
        case "l1" => "simplex"
        case "l2" => "multiplier"
        case _ => "simplex"
      }
    } else {
      //否则，按指定求解器选取
      lpSolver
    }
    println(s"solver name: $solverName")
    val solver = RereLPSolver.createLPSolver(obj1.length, obj2.length, obj2.length, solverName)
    var regWeight = regWeight0
    //自动化设置正则化项的权重
    if (regWeight == 0) regWeight = obj.length / 2.0
    val result: DenseVector[Double] = solver.solve(obj, m, b, regType, regWeight, obj1.length)._2.map(x => {
      var y: Double = x
      //防止值过大，过拟合
      if (x > maxValue) y = maxValue else if (x < 0) y = 0
      y
    }).slice(0, dim)
    val re: DenseMatrix[Double] = diag(RoundResult(sqrt(result)))
    //求解的实质是度量矩阵A，需要对对角矩阵开方
    return re
  }


  /**
    * 计算单个约束的相减项
    * 异类距离-同类距离 >= 1-松弛变量
    *
    * @param xi
    * @param xj
    * @param xk
    * @return
    */
  private def computeCoe(xi: Vector[Double], xj: Vector[Double], xk: Vector[Double]): DenseVector[Double] = {
    //同类距离
    val inter1 = xj - xi
    //异类距离
    val inter2 = xj - xk
    val coe1: Vector[Double] = inter1 *:* inter1
    val coe2: Vector[Double] = inter2 *:* inter2
    //异类点系数 - 同类点系数
    var inter = coe2 - coe1
    inter.toDenseVector
  }


}
