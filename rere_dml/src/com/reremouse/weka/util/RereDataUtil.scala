package com.reremouse.weka.util

import com.reremouse.dml.model.RereData
import com.reremouse.dml.spark.SparkHelper
import org.apache.spark.SparkContext
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, SparkSession}

object RereDataUtil {


  def buildRereDataAsSparkRDD(dataSet: Seq[RereData], spark:SparkContext):RDD[org.apache.spark.mllib.linalg.Vector]={
    //保存数据点的临时集合
    val ss=dataSet.map(x=>{
      org.apache.spark.mllib.linalg.Vectors.dense(x.getData.data)
    }).toArray
    val rows = spark.parallelize(ss)
    return rows
  }

  def buildRereDataAsSparkDataFrame(dataSet: Seq[RereData], spark:SparkSession):DataFrame={
    //保存数据点的临时集合
    var ss:Seq[Tuple3[String, org.apache.spark.ml.linalg.Vector, Int]] = null
    ss=dataSet.map(x=>{
      (x.getId, Vectors.dense(x.getData.data), x.getLabel())
    })
    val spark = SparkHelper.getSparkSession()
    //数据框
    val df = spark.createDataFrame(ss).toDF("id", "features", "label")
    df.cache()
    return df
  }

}
