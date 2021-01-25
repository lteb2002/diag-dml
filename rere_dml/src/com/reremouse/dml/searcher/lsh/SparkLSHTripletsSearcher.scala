package com.reremouse.dml.searcher.lsh


import java.util.stream.Collectors

import com.reremouse.dml.model.{RereData, Triplelet}
import com.reremouse.dml.searcher.ScalaTripletsSearcher
import com.reremouse.dml.spark.SparkHelper
import com.reremouse.dml.tool.DmlConfig
import org.apache.spark.ml.feature.BucketedRandomProjectionLSH
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.sql.Row

import scala.util.Random

/**
  * Created by RereMouse on 2018-01-18.
  */
class SparkLSHTripletsSearcher extends ScalaTripletsSearcher with Serializable {

  var tableNum = DmlConfig.noiseSize

  /**
    * 按照LMNN方法构建triplet
    *
    * @param dataSet 数据集合
    * @param max     最大加入数目
    * @param strict  只有在ij<ik时才加入
    */
  override def buildTriplets(dataSet: Seq[RereData], max: Int = 100000, strict: Boolean = false): Seq[Triplelet] = {
    val total = dataSet.size
    var trips = Seq[Triplelet]()
    var maps = Map[String, RereData]()
    //保存数据点的临时集合
    var ss: Seq[Tuple3[String,org.apache.spark.ml.linalg.Vector, Int]] = null
    ss = dataSet.map(x => {
      (x.getId,Vectors.dense(x.getData.data), x.getLabel())
    })
    maps = dataSet.map(x => {
      (x.getId -> x)
    }).toMap
    val spark = SparkHelper.getSparkSession()
    //数据框
    val df = spark.createDataFrame(ss).toDF("id","features", "label")
    df.cache()
    if (total > 200) {
      tableNum = total / 200
    }
    val brp = new BucketedRandomProjectionLSH()
      .setBucketLength(0.2)
      .setNumHashTables(2)
      .setInputCol("features")
      .setOutputCol("hashes")
    val lshModel = brp.fit(df)
    // Feature Transformation
    println("The hashed dataset where hashed values are stored in the column 'hashes':")
    var hashData = lshModel.transform(df).cache()
    hashData.show()
    //计算参与triplet筛选的数量，当数据规模过大时全选容易造成内存崩溃
    val takeNum = if (total > max * 10) max * 10 else total
    trips = hashData.rdd.map((row: Row) => {
      val hash = row.getAs[Seq[org.apache.spark.ml.linalg.Vector]]("hashes").map(_.toArray).flatMap(_.toList).mkString("-")
      //println(hash)
      val id = row.getAs[String]("id")
      //val data = row.getAs[org.apache.spark.ml.linalg.Vector]("features").toArray
      //val label = row.getAs[Int]("label")
      //重新转换为RereData
      (hash, Array(id))
    }).reduceByKey((a1, a2) => a1 ++ a2).flatMap(e => {
      val hash = e._1
      val ss = e._2
      println(hash + "    :::    " + ss.size)
      //默认最大点数为你类该方法的最大支持点数
      val ds=ss.map(s=>{maps.getOrElse(s,null)
      }).toList
      this.buildTripletsForLargeScale(ds,max,strict)
    }).take(takeNum)
    trips = this.filterTriplets(trips, strict, max) //按规则过滤三元组
    try {
      spark.close()
    } catch {
      case e: Exception => e.printStackTrace()
    }
    trips
  }


}
