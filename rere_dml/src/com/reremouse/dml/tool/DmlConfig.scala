package com.reremouse.dml.tool

object DmlConfig {

  // For the details of calculating the weights, refer to DmlWeight.scala
  final val sigma: Double = 1.0 / 4.5//必须为小数，否则运算将为0
  final val noiseSize = 5

}