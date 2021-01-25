package com.reremouse.dml.model

import java.io.Serializable
import java.util.UUID

import scala.beans.BeanProperty


/**
  *
  * @author reremouse
  */
class Triplelet extends Comparable[Triplelet] with Serializable {

  @BeanProperty var id = UUID.randomUUID.toString

  @BeanProperty var xi: RereData = null //同类最近点，与j

  @BeanProperty var xj: RereData = null //

  @BeanProperty var xk: RereData = null //异类最近点，与j

  @BeanProperty var jk = .0 //j与异类点间距离

  @BeanProperty var ji = .0 //j与同类点间距离

  @BeanProperty var weight = 1.0 //本triplet的权重


  override def toString: String = {
    //val s = "" + xi + ",    " + xj + ",    " + xk + ", jk: " + this.jk + ", ji: " + this.ji + ""
    val s = " jk: " + this.jk + ", ji: " + this.ji + ""
    s
  }

  def getGap(): Double = {
    this.getJk - this.getJi
  }

  //  override def compareTo(o: Triplelet): Int = {
  //    val oo = o.asInstanceOf[Triplelet]
  //    val gap1 = this.getGap()
  //    val gap2 = oo.getGap()
  //    if (gap1 < gap2) -1
  //    else if (gap1 > gap2) 1
  //    else 0
  //  }

  override def compareTo(o: Triplelet): Int = {
    val oo = o.asInstanceOf[Triplelet]
    if (this.getWeight < oo.getWeight) -1
    else if (this.getWeight > oo.getWeight) 1
    else 0
  }

}
