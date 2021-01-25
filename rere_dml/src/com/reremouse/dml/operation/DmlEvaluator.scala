package com.reremouse.dml.operation

import java.io.PrintWriter

import breeze.linalg.{DenseMatrix, diag, norm}
import breeze.numerics.pow
import com.reremouse.dml.model.Triplelet

/**
  * Created by RereMouse on 2018-01-18.
  */
class DmlEvaluator {

  var print:PrintWriter=null


  def this(pw:PrintWriter){
    this()
    print=pw
  }



  def evaluate(trips: Seq[Triplelet], dim: Int): Unit = {
    var distortError = 0.0d
    System.out.println("-------------------------------------------")
    System.out.println("Trips Size:" + trips.size)
    var error1 = 0d
    //记录同类总距离
    var error2 = 0d //记录异类总距离
    for (trip <- trips) {
      val d1 = trip.getXj.getData
      val d2 = trip.getXi.getData
      val d3 = trip.getXk.getData
      error1 += pow(norm( d1 - d2), 2)
      error2 += pow(norm(d1 -  d3), 2)
    }
    distortError = error1 / error2
    val log="Gap of same labels:" + error1 + ", Gap of different labels:" + error2 + ", Distort error:" + distortError
    println(log)
    print.println(log)
    println("-------------------------------------------")
  }

  def evaluate(trips: Seq[Triplelet], dim: Int, proj: DenseMatrix[Double]): Unit = {
    var distortError = 0d
    val te = trips(0)
    System.out.println("-------------------------------------------")
    System.out.println("Evaluation trips size:" + trips.size)
    var error1 = -0d
    var error2 = 0d
    for (trip <- trips) {
      val d1 = trip.getXj.getData
      val d2 = trip.getXi.getData
      val d3 = trip.getXk.getData
      error1 += pow(norm(proj * d1 - proj * d2), 2)
      error2 += pow(norm(proj * d1 - proj * d3), 2)
    }
    println("Projection matrix:\n" + proj)
    print.println("Projection vector:")
    print.println(diag(proj).toArray.mkString(","))
    distortError = error1 / error2
    val log="Gap of same labels:" + error1 + ", Gap of different labels:" + error2 + ", Distort error:" + distortError
    println(log)
    print.println(log)
    println("-------------------------------------------")
  }

}
