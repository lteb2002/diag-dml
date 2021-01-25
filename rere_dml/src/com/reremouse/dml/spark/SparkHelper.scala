package com.reremouse.dml.spark

import org.apache.spark.sql.SparkSession

/**
  * Created by Administrator on 2017/12/22.
  */
object SparkHelper {

  /**
    *
    * @param ifShowLog
    * @return
    */
  def getSparkSession(ifShowLog: Boolean): SparkSession = {
    System.setProperty("SPARK_WORKER_MEMORY", "60G")
    System.setProperty("SPARK_WORKER_INSTANCES", "4")
    val spark = SparkSession
      .builder
      .config("spark.driver.maxResultSize", "8G")
      .config("spark.driver.memory", "60G")
      .config("spark.executor.memory", "50G")
      .config("spark.executor-cores", "24")
      .config("spark.local.dir", "C:/tmp")
      .appName("Spark Example")
      .master("local[*]")
      .getOrCreate()
    if (!ifShowLog) {
      spark.sparkContext.setLogLevel("INFO")
    }
    spark
  }

  def getSparkSession(): SparkSession = {
    getSparkSession(true)
  }


  def main(args:Array[String]):Unit={
    val spark=SparkHelper.getSparkSession()
    println(spark)
  }

}
