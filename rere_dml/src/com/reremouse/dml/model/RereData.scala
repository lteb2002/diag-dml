package com.reremouse.dml.model

import java.util.UUID
import breeze.linalg._
import scala.beans.BeanProperty

/**
  * Created by RereMouse on 2018-01-17.
  */
class RereData extends Serializable{

  @BeanProperty var id:String = _
  @BeanProperty var data = DenseVector[Double]()
  @BeanProperty var label: Int = _


  def this(idx:String,v: Array[Double], l: Int) = {
    this()
    id=idx
    data = DenseVector(v)
    label = l
  }
  def this(v: Array[Double], l: Int) = {
    this(UUID.randomUUID().toString,v,l)
  }

  /**
    * 使用投影矩阵对本数据点进行变换
    *
    * @param prj
    * @return
    */
  def projectWithMatrix(prj: DenseMatrix[Double]): RereData = {
    this.data = prj * data
    this
  }

  @Override
  override def toString(): String ={
    val str="Label:"+label+", v:"+data.toString
    str
  }

}
