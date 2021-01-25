package com.reremouse.dml.svd


import breeze.linalg.{DenseVector, sum}
import com.reremouse.dml.model.{RereData, RereDataCenter}
import com.reremouse.dml.spark.SparkHelper
import com.reremouse.util.RereLogger
import com.reremouse.weka.util.RereDataUtil
import org.apache.spark.ml.feature.PCA
import org.apache.spark.mllib.linalg.SingularValueDecomposition
import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.sql.Row

/**
  * Created by RereMouse on 2018-01-21.
  */
object SVDTransformerWithSpark {

  val logger = RereLogger.getLogger(this.getClass)

  /**
    *
    * @param dataSet
    * @return
    */
  def transform(dataSet: Seq[RereData]): Seq[RereData] = {
    val k = dataSet.length
    transform(dataSet, k)
  }

  /**
    *
    * @param dataSet
    * @param k
    * @return
    */
  def transform(dataSet: Seq[RereData], k: Int): Seq[RereData] = {
    transformAndReturnSV(dataSet, k)._1
  }

  /**
    *
    * @param dataSet
    * @param k
    * @return
    */
  def transformAndReturnSV(dataSet: Seq[RereData], k: Int): (Seq[RereData], DenseVector[Double]) = {
    val dim = dataSet(0).getData.length
    val len = dataSet.length
    //    val spark = SparkHelper.getSparkSession().sparkContext
    //    val rdd = RereDataUtil.buildRereDataAsSparkRDD(dataSet, spark)
    //    val mat: RowMatrix = new RowMatrix(rdd)

    //    // Compute the top 5 singular values and corresponding singular vectors.
    //    val svd: SingularValueDecomposition[RowMatrix, org.apache.spark.mllib.linalg.Matrix] = mat.computeSVD(5, computeU = true)
    //    val U: RowMatrix = svd.U  // The U factor is a RowMatrix.
    //    val s: Vector = svd.s     // The singular values are stored in a local dense vector.
    //    val V: Matrix = svd.V
    val spark = SparkHelper.getSparkSession()
    val df = RereDataUtil.buildRereDataAsSparkDataFrame(dataSet, spark)
    val max=100
    val pca = new PCA()
      .setInputCol("features")
      .setOutputCol("pcaFeatures")
      .setK(if(dim>max)max else dim)
      .fit(df)
    val s = pca.explainedVariance.values
    val total = sum(s)
    var eff = dim
    var i = 0

    while (eff == dim && i<=max) {
      val sub = sum(s.slice(0, i + 1))
      if (sub / total > 0.95) {
        eff = i
      }
      i += 1
    }
    println(s"effective num: $eff")
    val result = pca.transform(df)
    result.show()
    println("singular values:"+s.mkString("[",",","]"))
    val newData = result.rdd.map((row: Row) => {
      val id = row.getAs[String]("id")
      //降维
      val data = row.getAs[org.apache.spark.ml.linalg.Vector]("pcaFeatures").toArray.slice(0,eff)
      val label = row.getAs[Int]("label")
      new RereData(id, data, label)
    }).collect()
    (newData, DenseVector(s))
  }

  def main(args: Array[String]): Unit = {

    val path = "H:\\dml_experiments\\"
    //val fNames = Array("ele_vector_eq_sample_500_500")
    val fNames = Array("HAPT")
    for (fn <- fNames) {
      try {
        val input = path + fn + ".arff"
        val output = path + fn + "_pca.arff"
        val logFile = path + "logs\\" + fn + ".log"
        val dc = new RereDataCenter
        val data = dc.loadWekaData(input)
        val newData = SVDTransformerWithSpark.transform(data, 10)
        val d1 = newData(0).data
        val d2 = newData(1).data
        val d3 = newData(2).data
        dc.exportWekaData(output, newData)
        //println(d1)
        //println(d2)
        //数据与数据乘积不是零，维度正交，乘积才是0
        //println(d1.t * d2)
        //println(d1.t * d3)


      } catch {
        case ex: Exception =>ex.printStackTrace()
      }
    }
  }

}
