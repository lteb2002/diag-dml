package com.reremouse.dml.searcher

import breeze.linalg._
import com.reremouse.dml.model.{RereData, Triplelet}
import com.reremouse.dml.tool.DmlConfig
import com.reremouse.util.RereLogger

import scala.collection.parallel.ParSeq
import scala.util.Random
import scala.collection.mutable.ListBuffer

/**
  * Created by RereMouse on 2018-01-18.
  */
class ScalaTripletsSearcher extends TripletsSearcher {

  val logger = RereLogger.getLogger(this.getClass)
  //丢弃的严重变形数据点，抗噪声
  val dropNum = DmlConfig.noiseSize

  /**
    * 按照LMNN方法构建triplet
    *
    * @param dataSet 数据集合
    * @param max     最大加入数目
    * @param strict  只有在ij<ik时才加入
    */
  def buildTriplets(dataSet: Seq[RereData], max: Int = 10000, strict: Boolean = false): Seq[Triplelet] = {
    println("Prepare to handle data size:"+dataSet.size)
    val trips = buildTripletsFromRereDataSeq(dataSet, max)
    this.filterTriplets(trips, strict, max)
  }

  /**
    * 按照LMNN方法为大规模数据集构建triplet
    *
    * @param dataSet0 数据集合
    * @param max     最大加入数目
    * @param strict  只有在ij<ik时才加入
    */
  def buildTripletsForLargeScale(dataSet0: Seq[RereData], max: Int = 10000, strict: Boolean = false): Seq[Triplelet] = {
    println("Prepare to handle data size:" + dataSet0.size)
    var dataSet: Seq[RereData] = dataSet0
    if (max < dataSet.size) { //随机抽取max个点
      println("Sub dataset are selected. Total:" + dataSet0.size + ",selected:" + max)
      //为了保持可重复的实验结果，shuffle带有seed
      Random.setSeed(100L)
      dataSet = Random.shuffle(dataSet0).slice(0, max)
    } else {
      dataSet = dataSet0
    }
    val trips = buildTripletsFromRereDataSeq(dataSet, max)
    trips
  }

  /**
    * @param dataSet0 所有数据或者HASH组内数据
    * @return
    */
  protected def buildTripletsFromRereDataSeq(dataSet0: Seq[RereData], max: Int = 10000): Seq[Triplelet] = {
    var dataSet: Seq[RereData] = dataSet0
    //    if (max < dataSet0.size) { //随机抽取max个点
    //      println("Sub dataset are selected. Total:" + dataSet0.size + ",selected:" + max)
    //      //为了保持可重复的实验结果，shuffle带有seed
    //      Random.setSeed(100L)
    //      dataSet = Random.shuffle(dataSet0).slice(0, max)
    //    } else { dataSet = dataSet0 }

    var trips = ListBuffer[Triplelet]()
    val classMap = dataSet.map(x => {
      (x, x.getLabel)
    }).groupBy(_._2) //按label分组并形成Map: {label1 -> (RereData,label1),label2 -> (RereData,label2),...}

    val keys = classMap.keySet.toList
    //将Set变换为List,Set不能通过下标取元素
    val cs = keys.size //将不同类别的数据分别放到不同的dataClass
    println("Classes Size:" + cs)
    for (i <- 0 until cs) { //比较不同类间的距离
      //label
      val key: Int = keys(i)
      //i类的所有数据，并行数组
      val same = classMap(key).map(x => {
        x._1 //二元组中仅返回第一项
      })
      //其它类别的所有数据，并行Par
      var other = ListBuffer[RereData]()
      for (j <- 0 until cs) {
        if (i != j)
          other ++= classMap(keys(j)).map(x => {
            x._1
          })
      }
      //并行计算Quadruplet
      trips ++= same.map(xci => {
        //使用MapReduce方法找出异类数据中的最近邻
        var (rd1: RereData, miniDis1) = if (other.isEmpty) (new RereData(), -1.0) else other.par.map(x => {
          var dis = norm(xci.getData - x.getData)
          if (dis == 0) dis = -1
          (x, dis)
        }).reduce((e1, e2) => {
          if (e1._2 <= e2._2) e1 else e2
        })
        //使用MapReduce方法找出同类中的最近点
        var (rd2: RereData, miniDis2) = if (same.isEmpty) (new RereData(), -1.0) else same.par.map(x => {
          var dis = norm(xci.getData - x.getData)
          if (dis == 0) dis = -1 //可能与自身比较
          (x, dis)
        }).reduce((e1, e2) => {
          if (e1._2 <= e2._2 && e1._2 != -1) e1 else e2
        })
        val q = new Triplelet()
        q.setXi(rd2)
        q.setXj(xci)
        q.setXk(rd1)
        q.setJi(miniDis2)
        q.setJk(miniDis1)
        q
      })
      //本类中的所有点都找到了最近的异类点
    }
    //删除可能有空项的三元组
    trips = trips.filter(x => {
      x.getJi != -1.0 && x.getJk != -1.0 && (!x.getJi.isInfinite) && (!x.getJk.isInfinite)
    })
    trips = Random.shuffle(trips) //打乱顺序，后续步骤可能要随机抽样
    trips.toSeq
  }

  /**
    * 按规则过滤三元组
    *
    * @param trips0 original triplets
    * @param strict if constrain that xij<xjk
    * @param max    the max triplet number used for DML learning
    * @return
    */
  protected def filterTriplets(trips0: Seq[Triplelet], strict: Boolean, max: Int): Seq[Triplelet] = {
    val drop = if (trips0.size > dropNum) dropNum else trips0.size - 1
    //计算丢弃的三元组点数
    var trips = trips0.sortWith(_.compareTo(_) < 0)
    val sl = trips.slice(0, drop).map(x => {
      x.getJk - x.getJi
    }).mkString(",")
    println("Gap order:" + sl)
    //去掉变形过重的drop个点，可能是嗓音
    trips = trips.slice(drop, trips.size)
    if (strict) {
      //并行筛选
      trips = trips.par.filter(t => {
        t.getJk < t.getJi
      }).toList
    }
    //打乱顺序，后续步骤可能要随机抽样
    //Random.setSeed(100L)
    //trips = Random.shuffle(trips)
    //本段主要为其它继承此类的子类核实triplets数量
    if (max < trips.size) { //随机抽取max个点
      println("Sub triplets are selected. Total:" + trips.size + ",selected:" + max)
      trips = trips.sorted.reverse
      trips = trips.slice(0, max)
    }
    trips
  }

}
