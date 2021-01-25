package com.reremouse.util

import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by RereMouse on 2018-01-18.
  */
object RereLogger {

    //System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "trace")

  /**
    * 获得logger
    * @param clazz
    * @return
    */
  def getLogger(clazz:Class[_]):Logger={
    val logger=LoggerFactory.getLogger(clazz)
    logger
  }

}
