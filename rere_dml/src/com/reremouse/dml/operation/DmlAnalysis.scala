package com.reremouse.dml.operation

import java.io._

import breeze.linalg.DenseMatrix
import com.reremouse.dml.model.{RereData, RereDataCenter}
import com.reremouse.dml.searcher.TripletsSearcher
import com.reremouse.dml.solver.dml.RereDmlSolver

import scala.util.Random

/**
  * Created by RereMouse on 2018-01-18.
  */
object DmlAnalysis {


  /**
    * 对原数据集进行变换
    *
    * @param data
    * @param prj
    * @return
    */
  def transformData(data: Seq[RereData], prj: DenseMatrix[Double]): Seq[RereData] = {
    val newData = data.toParArray.map(x => {
      x.projectWithMatrix(prj)
    }).toList
    newData
  }

  /**
    *
    */
  def doDml(file1: String, file2: String,
            maxDataSize: Int,
            maxTriplets: Int, strict: Boolean,
            logFile: String,
            lpSolver: String,
            regType: String, regWeight: Double): Unit = {
    //数据维度
    var k: Int = -1
    val dataCenter = new RereDataCenter()
    var data = dataCenter.loadWekaData(file1)
    //data = Random.shuffle(data).slice(0,300000)
    //数据量
    val num = data.size
    if (k == -1) {
      try {
        k = data(0).data.length
      } catch {
        case ex: Exception => ex.printStackTrace()
      }
    }
    //data=SVDTransformer.transform(data,k)
    val time1 = System.currentTimeMillis()
    var pairM = "pairwise"
    if (num > maxDataSize) pairM = "lsh"
    val trips = TripletsSearcher.createSearcher(pairM).buildTriplets(data, maxTriplets, strict)
    val dim = trips(0).getXj.getData.length
    val totalTrips = trips.size
    val time2 = System.currentTimeMillis()
    println("Total triplets:" + totalTrips)
    //记录执行过程和执行结果
    val os = new BufferedOutputStream(new FileOutputStream(logFile))
    val pw = new PrintWriter(os)
    pw.println("Total triplets:" + totalTrips)
    val eval = new DmlEvaluator(pw)
    //评估当前的同异类点状况
    eval.evaluate(trips, dim)
    //DML求解器（内部调用其它优化算法）
//    println(regWeight)
    val prjMatrix = RereDmlSolver.createSolver.computeProjectionMatrix(trips, lpSolver, regType, regWeight)
    val time3 = System.currentTimeMillis()
    //评估变换之后的同异类点状况
    eval.evaluate(trips, dim, prjMatrix)
    val interval1 = (time2 - time1) / 1000.0f
    val interval2 = (time3 - time2) / 1000.0f
    val interval3 = (time3 - time1) / 1000.0f
    pw.println("Search triplets time:" + interval1)
    println("Search triplets time:" + interval1)
    pw.println("LP solving time:" + interval2)
    println("LP solving time:" + interval2)
    pw.println("Total time:" + interval3)
    println("Total time:" + interval3)
    pw.close()
    os.close()
    data = transformData(data, prjMatrix)
    dataCenter.exportWekaData(file2, data)
  }

  def main(args: Array[String]): Unit = {

    val path = "H:\\dml_experiments\\dml_net\\"
//    val fNames = Array("bank_marketing","bank-additional","gao_xin_numeric_n2b_utf-8","BJ_DEPOSIT_RANKs","credit_risk")
    val fNames = Array("diabetes")
    //val fNames = Array("iris_svd","wine_svd","diabetes_svd","segment_svd","waveform_svd","letter-recognition_svd","magic04_svd","spambase_svd","credit_card_svd","shuttle_svd","HAPT_svd")


    // valid values of lpSolver include 'pf', 'admm' and 'simplex'. 'pf' and 'admm' are for 'L2' reg., while simplex is for 'none' or 'L1' reg.
    // Since pf and admm solver are implemented with Julia language, you should start Julia server first (run run_ju4ja_for_rere_dml.jl) if you use either of the two solvers.
//    val lpSolver = "pf"
//    val lpSolver = "admm"
    val lpSolver = "simplex"
    //valid value of regType is 'none' or 'l1' or 'l2'.
    val regTypes: Array[String] = Array("l1")
    //val regType: String = "none"
//    val regWeights: Array[Double] = Array(0.1) //设置为0时将自动计算正则化权重
//    val regWeights: Array[Double] = Array(0.001,0.01,0.1,1,10,100,1000) //设置为0时将自动计算正则化权重
    val regWeights: Array[Double] = Array(1) //设置为0时将自动计算正则化权重

    for (fn <- fNames) {
      for (regType <- regTypes) {
        for (reg <- regWeights) {
          try {
            val input = path + fn + ".arff"
            val output = path + fn + "_" + regType + "_" + reg + "_dml.arff"
            val logPath = path + "logs\\"
            val logPathFile = new File(logPath)
            if (!logPathFile.exists()) logPathFile.mkdir()
            val logFile = logPath + fn + "_" + regType + "_" + reg + ".log"
            val maxDataSize = 50000 //高于此数目将调用Spark构建Triplet
            val maxTriplets = 10000 //10000
            val strict = false
            doDml(input, output, maxDataSize, maxTriplets, strict, logFile, lpSolver, regType, reg)
          } catch {
            case ex: Exception => ex.printStackTrace()
          }
        }
      }
    }
  }

}
