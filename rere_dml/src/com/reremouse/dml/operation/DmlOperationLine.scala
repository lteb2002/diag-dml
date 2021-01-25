package com.reremouse.dml.operation

import java.io.{BufferedOutputStream, FileOutputStream, PrintWriter}

import com.reremouse.dml.model.RereDataCenter
import com.reremouse.dml.operation.DmlAnalysis.transformData
import com.reremouse.dml.searcher.TripletsSearcher
import com.reremouse.dml.solver.dml.RereDmlSolver

/**
  * 度量学习流水线（封装了所有基本操作）
  * all necessary operations to conduct DML
  * Created by RereMouse on 2018-01-22.
  */
object DmlOperationLine {

  def operateWithLpDml(input:String,output:String,regType: String , regWeight: Double):Unit={
    val dataCenter=new RereDataCenter()
    var data=dataCenter.loadWekaData(input)
    //data=SVDTransformer.transform(data)
    val trips=TripletsSearcher.createSearcher().buildTriplets(data)
    val dim = trips(0).getXj.getData.length
    val totalTrips = trips.size
    println("Total triplets:"+totalTrips)
    val logFile="d:\\aa.log"
    val os=new BufferedOutputStream(new FileOutputStream(logFile))
    val pw=new PrintWriter(os)
    val eval=new DmlEvaluator(pw)
    //评估当前的同异类点状况
    eval.evaluate(trips, dim)
    val prjMatrix=RereDmlSolver.createSolver.computeProjectionMatrix(trips,"auto",regType,regWeight)
    //评估变换之后的同异类点状况
    eval.evaluate(trips, dim, prjMatrix)
    data=transformData(data,prjMatrix)
    dataCenter.exportWekaData(output,data)
  }

}
