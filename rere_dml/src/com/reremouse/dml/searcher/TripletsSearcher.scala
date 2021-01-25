package com.reremouse.dml.searcher

import com.reremouse.dml.model.{ RereData, Triplelet }
import com.reremouse.dml.searcher.lsh.SparkLSHTripletsSearcher

/**
 * Created by RereMouse on 2018-01-19.
 */
trait TripletsSearcher {

  /**
   * 按照LMNN方法构建triplet
   *
   * @param dataSet 数据集合
   * @param k       最大加入数目
   * @param strict  只有在ij<ik时才加入
   */
  def buildTriplets(dataSet: Seq[RereData], max: Int=30000, strict: Boolean=false): Seq[Triplelet]

  /**
   * 使用默认参数计算
   * @param dataSet
   * @return
   */
  def buildTriplets(dataSet: Seq[RereData]): Seq[Triplelet] = {
    val k = dataSet.length
    this.buildTriplets(dataSet, k, false)
  }

}

object TripletsSearcher {

  /**
   * 工厂方法，返回一个三元组搜索器
   * 可能是Scala并行方法或者Spark方法
   * @param method:进行Triplet搜索的方法，可用方法为：pairwise||lsh
   * @return
   */
  def createSearcher(method:String="pairwise"): TripletsSearcher = {
    var searcher:TripletsSearcher =null
    if("lsh".equals(method.toLowerCase())){
      searcher = new SparkLSHTripletsSearcher()
    }else{
      searcher = new ScalaTripletsSearcher()
    }
    //val searcher = new NeuralGasTripletsSearcher()
    return searcher
  }
}