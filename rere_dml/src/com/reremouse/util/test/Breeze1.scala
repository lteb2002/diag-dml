package com.reremouse.util.test

import breeze.linalg.DenseVector

import com.github.fommil.netlib.BLAS

import org.slf4j.LoggerFactory




/**
  * 测试加载的BLAS库
  */
object Breeze1 {

  def main(args:Array[String]): Unit = {

    println("Init logging...")
    val log = LoggerFactory.getLogger("main")
    println("Starting...")
    val b = BLAS.getInstance()
    println(s"BLAS = $b")
    val v = DenseVector(1,2,3,4)
    println("Ending.")
  }
}

