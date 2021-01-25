package com.reremouse.dml.solver.lp

import breeze.linalg.{DenseMatrix, DenseVector}

object LPCoordinator {

  /**
    *
    * @param const
    * @param obj
    * @return
    */
  def transformBiggerEqualsThanAsEquals(const: DenseMatrix[Double],
                                        obj: DenseVector[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
    val A = DenseMatrix.horzcat(const, -DenseMatrix.eye[Double](const.rows))
    val c: DenseVector[Double] = DenseVector.vertcat(obj, DenseVector.zeros[Double](A.rows))
    (A, c)
  }

  def transformLessEqualsThanAsEquals(const: DenseMatrix[Double],
                                        obj: DenseVector[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
    val A = DenseMatrix.horzcat(const, DenseMatrix.eye[Double](const.rows))
    val c: DenseVector[Double] = DenseVector.vertcat(obj, DenseVector.zeros[Double](A.rows))
    (A, c)
  }

  /**
    *
    * @param constraints
    * @param obj
    * @return
    */
  def transformBiggerEqualsThanAsAdmmSegments(constraints: DenseMatrix[Double],
                                              obj: DenseVector[Double],
                                              mainVnum: Int ,
                                              s1Vnum: Int ,
                                              s2Vnum: Int )
  : (DenseMatrix[Double], DenseMatrix[Double], DenseVector[Double], DenseVector[Double], Int, Int
    ) = {
    val rows = constraints.rows
    var mainNum = 0 //主变量数
    if (mainVnum == 0) {
      mainNum = constraints.cols - rows
    }else{
      mainNum=mainVnum
    }

    val main = constraints(::, 0 until mainNum)
    val slack1 = constraints(::, mainNum until mainNum+s1Vnum)
    val slack2 = DenseMatrix.eye[Double](s2Vnum)
    val m1Num: Int = Math.ceil(mainNum / 2.0f).toInt
    val m2Num: Int = Math.round(slack1.cols.toInt / 2.0f)
    val m3Num: Int = Math.floor(slack2.cols.toInt / 2.0f).toInt
    println(s"m1 num:$m1Num, m2 num:$m2Num, m3 num:$m3Num")
    val A11 = -main(::, 0 until m1Num)
    val A21 = -main(::, m1Num until main.cols)
    val A12 = -slack1(::, 0 until m2Num)
    val A22 = -slack1(::, m2Num until slack1.cols)
    val A13 = slack2(::, 0 until m3Num)
    val A23 = slack2(::, m3Num until slack2.cols)
    val A1 = if(m2Num==0&&m3Num==0)A11 else DenseMatrix.horzcat(A11, A12, A13)
    val A2 = if(m2Num==0&&m3Num==0) A21 else DenseMatrix.horzcat(A21, A22, A23)
    val cx1 = obj(0 until mainNum) //主目标函数部分
    val cx2 = obj(mainNum until obj.length) //松驰变量罚项
    val cx3 = DenseVector.zeros[Double](rows) //检驰变量0系数项
    val c11 = cx1(0 until m1Num)
    val c21 = cx1(m1Num until main.cols)
    val c12 = cx2(0 until m2Num)
    val c22 = cx2(m2Num until slack1.cols)
    val c13 = cx3(0 until m3Num)
    val c23 = cx3(m3Num until slack2.cols)
    val c1 = if(m2Num==0&&m3Num==0) c11 else DenseVector.vertcat(c11, c12, c13)
    val c2 = if(m2Num==0&&m3Num==0) c21 else DenseVector.vertcat(c21, c22, c23)
    println("c1:" + c1)
    println("c2:" + c2)

    println("A1:\n" + A1)
    println("A2:\n" + A2)
    (A1, A2, c1, c2, m1Num, mainNum - m1Num)
  }


}
