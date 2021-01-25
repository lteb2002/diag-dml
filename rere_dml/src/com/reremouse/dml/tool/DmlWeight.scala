package com.reremouse.dml.tool

/**
  * Created by RereMouse on 2018-01-18.
  */
object DmlWeight {

  import com.reremouse.dml.model.Triplelet

  val sigma: Double = DmlConfig.sigma //必须为小数，否则运算将为0


  def computeWeight(trip: Triplelet): Double = {
    //异类距离与同类距离的和
    //val inter = trip.getJk + trip.getJi
    val inter = trip.getJk - trip.getJi
    //val inter= trip.getJk / trip.getJi
    //计算该数据点权重
    val w = Math.exp(-Math.abs(inter) / sigma)
    trip.setWeight(w)
    w
  }
}
