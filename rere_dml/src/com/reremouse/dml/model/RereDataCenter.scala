package com.reremouse.dml.model

import java.io.File

import breeze.linalg.dim
import com.reremouse.util.RereLogger
import com.reremouse.weka.util.WekaUtil
import weka.core.converters.{AbstractFileLoader, AbstractFileSaver, ArffLoader, ArffSaver, CSVLoader, CSVSaver, Saver}
import weka.core.{DenseInstance, Instance, Instances}
import weka.filters.Filter
import weka.filters.unsupervised.attribute.Normalize

/**
  * Created by RereMouse on 2018-01-19.
  */
class RereDataCenter {
  val logger = RereLogger.getLogger(this.getClass)

  var attrNum = -1 //属性数（包括标签列）
  var insNum = 0 //实例数

  private var ins: Instances = null
  private var structure: Instances = null


  /**
    * 加载数据
    *
    * @param file
    * @return
    */
  def loadWekaData(file: String, ifNomalize: Boolean = true): Seq[RereData] = {
    logger.info("Load data from:{}", file)
    println(file)
    var data = Seq[RereData]()
    try {
      val file2 = new File(file)
      var loader: AbstractFileLoader = new ArffLoader
      if (file.endsWith(".csv")) {
//        println("csv")
        loader = new CSVLoader()
      } else if (file.endsWith(".arff")) {
        loader = new ArffLoader()
//        println("arff")
      }
      loader.setFile(file2)
      ins = loader.getDataSet
      structure = loader.getStructure
      ins.setClassIndex(ins.numAttributes - 1)
      ins = normalize(ins)
      insNum = ins.numInstances
      attrNum = ins.numAttributes()
      logger.info("Data size:{}", insNum)
      data = ins.toArray.map(inx => {
        val in = inx.asInstanceOf[Instance]
        val value = in.toDoubleArray
        val label = in.classValue.asInstanceOf[Int]
        val v = value.slice(0, attrNum - 1)
        val rd = new RereData(v, label)
        //println(v.mkString(","))
        rd
      })
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
    logger.info("Data load finished.")
    data
  }


  /**
    *
    * @param data
    * @return
    */
  private def normalize(data: Instances): Instances = {
    val convert = new Normalize()
    var data2: Instances = null
    try {
      convert.setInputFormat(data)
      data2 = Filter.useFilter(data, convert)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
    data2
  }

  /**
    *
    * @param file
    * @param data
    */
  def exportWekaData(file: String, data: Seq[RereData]): Unit = {
    try {
      val newDim = data(0).data.length
      var newStruct = structure
      //如果数据被降维，取表头子集
      if (structure.size() != newDim) {
        newStruct = WekaUtil.rebuildStructure(structure, newDim)
        attrNum = newDim + 1
      }
      var saver: AbstractFileSaver = new ArffSaver()
      if (file.endsWith("csv"))
        saver = new CSVSaver()
      else if (file.endsWith("arff"))
        saver = new ArffSaver()
      saver.setFile(new File(file))
      saver.setRetrieval(Saver.INCREMENTAL)
      saver.setStructure(newStruct)
      for (dc <- data) {
        val cla = dc.getLabel
        val in = new DenseInstance(attrNum)
        val v = dc.getData
        for (i <- 0 until v.length) in.setValue(i, v(i))
        in.setValue(attrNum - 1, cla)
        saver.writeIncremental(in)
      }
      saver.getWriter.close()
      //pw.flush();
      //pw.close();
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }

}

object RereDataCenter {
  def main(args: Array[String]): Unit = {
    val file = "G:\\dml_experiments\\iris.csv"
    val dc = new RereDataCenter
    val data = dc.loadWekaData(file)
    data.foreach(x => {
      println(x.toString())
    })
  }

}
