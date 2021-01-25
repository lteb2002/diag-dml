package com.reremouse.dml.solver.dml

import breeze.linalg._
import com.reremouse.dml.model.{DistanceMetricsSum, RereData, Triplelet}
import com.reremouse.dml.solver.lp.RereLPSolver
import com.reremouse.dml.tool.DmlWeight
import com.reremouse.util.RereLogger

/**
  * Created by RereMouse on 2018-01-18.
  */
class RereDmlSdpSolver extends RereDmlSolver {


  val maxConstraints: Int = 100000
  val maxValue: Double = 100000

  val u: Double = 9999 //松弛变量和的系数


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
                              regWeight: Double): DenseMatrix[Double] = {
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
    //约束数和松弛变量数
    val size = targets.size

    //原变量约束系数矩阵，按行向量数组构建
    val m1: DenseMatrix[Double] = DenseMatrix.zeros(size, dim * (dim + 1) / 2)
    for (i <- 0 until size) {
      m1(i, ::) := targets(i).t
    }
    //约束矩阵中的松弛变量系数
    val m2 = DenseMatrix.eye[Double](size)
    //全约束矩阵
    var m = DenseMatrix.horzcat(m1, m2)
    //构建约束变量大于0的矩阵
    val m0 = DenseMatrix.eye[Double](dim * (dim + 1) / 2 + size)
    m = DenseMatrix.vertcat(m, m0)
    logger.info("Constrain matrix:\n" + m(::, 0 until dim * (dim + 1) / 2))
    //原变量系数
    val obj1: DenseVector[Double] = DenseVector.zeros[Double](dim * (dim + 1) / 2)
    var ind1: Int = 0
    for (i <- 0 until dim) {
      for (j <- i until dim) {
        if (i == j)
          obj1(ind1) = cc(i, j)
        else
          obj1(ind1) = 2 * cc(i, j)
        ind1 += 1
      }
    }
    //目标函数中的松弛变量系数
    val obj2: DenseVector[Double] = DenseVector.ones[Double](size) * u
    //全目标函数系数
    val obj = DenseVector.vertcat(obj1, obj2)
    logger.info("obj:" + obj(0 until dim * (dim + 1) / 2))
    //约束常量
    var b = DenseVector.ones[Double](size)
    //所有变量大于等于0
    val b0 = DenseVector.zeros[Double](dim * (dim + 1) / 2 + size)
    b = DenseVector.vertcat(b, b0)
    logger.info("b:" + b(0 until dim))
    //线性规划求解器
    val solver = RereLPSolver.createLPSolver(obj1.length, obj2.length, obj2.length,lpSolver)
    val result: DenseVector[Double] = solver.solve(obj, m, b,regType,regWeight)._2.map(x => {
      //防止值过大，过拟合
      if (x > maxValue) maxValue else x
    }).slice(0, dim * (dim + 1) / 2)
    val re: DenseMatrix[Double] = DenseMatrix.zeros[Double](dim, dim)
    var ind2: Int = 0
    for (i <- 0 until dim) {
      for (j <- i until dim) {
        if (i == j)
          re(i, j) = result(ind2)
        else {
          re(i, j) = result(ind2) / 2d
          re(j, i) = result(ind2) / 2d
        }
        ind2 += 1
      }
    }
    //求解的实质是度量矩阵A，需要向正定核投影并进行乔里斯基分解，取上三角阵为投影矩阵
    return orthogonalProject(re)
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
    val dim = xi.length
    //同类距离
    val inter1 = xj - xi
    val x1 = DenseVector.zeros[Double](dim * (dim + 1) / 2)
    var ind1: Int = 0
    for (i <- 0 until dim) {
      for (j <- i until dim) {
        if (i == j)
          x1(ind1) = inter1(i) * inter1(j)
        else
          x1(ind1) = 2 * inter1(i) * inter1(j)
        ind1 += 1
      }
    }
    //异类距离
    val inter2 = xj - xk
    val x2 = DenseVector.zeros[Double](dim * (dim + 1) / 2)
    var ind2: Int = 0
    for (i <- 0 until dim) {
      for (j <- i until dim) {
        if (i == j)
          x2(ind2) = inter2(i) * inter2(j)
        else
          x2(ind2) = 2 * inter2(i) * inter2(j)
        ind2 += 1
      }
    }
    //异类点系数 - 同类点系数
    val inter = x2 - x1
    inter.toDenseVector
  }

  def orthogonalProject(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val es = eigSym(matrix)
    val er = es.eigenvalues
    val ev = es.eigenvectors
    val err: DenseVector[Double] = DenseVector.zeros[Double](er.length)
    for (i <- 0 until er.length) {
      err(i) = {
        if (er(i) < 0) 0 else er(i)
      }
    }
    //val d = diag(er)
    val d2 = diag(err)
    //val newM1 = ev * d * ev.t
    val newM2 = ev * d2 * ev.t
    val subDiag = cholesky(newM2)
    subDiag.t
  }


}

object RereDmlSDPSolver {
  def main(args: Array[String]): Unit = {
    val matrix: DenseMatrix[Double] = DenseMatrix((3d, 1d, 3d), (1d, -2d, 1d), (3d, 1d, 8d))
    val es = eigSym(matrix)
    val er = es.eigenvalues
    val ev = es.eigenvectors
    val err: DenseVector[Double] = DenseVector.zeros[Double](er.length)
    for (i <- 0 until er.length) {
      err(i) = {
        if (er(i) < 0) 0 else er(i)
      }
    }
    val d = diag(er)
    val d2 = diag(err)
    val newM1 = ev * d * ev.t
    val newM2 = ev * d2 * ev.t
    println(er)
    println(ev)
    println("----------------------")
    println(d)
    println("----------------------")
    println(d2)
    println("----------------------")
    println(newM1)
    println("----------------------")
    println(newM2)
    val t = cholesky(newM2)
    println("----------------------")
    println(t)
    println("----------------------")
    println(t * t.t)

  }
}
