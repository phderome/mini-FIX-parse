package com.deromefintech.dictionary

import play.api.libs.json._

/**
  * Created by philippederome on 2016-10-08.
  */
trait PlayJSReads {
  /**
    *
    * @param obj a parsed JSON object as JsValue
    * @param path the path to use to select elements of the collection we need within obj
    * @param rds the way to read the type we need that PLAY needs for parsing each individual element
    * @tparam T the type parameter of the collection we need
    * @return the sequence of elements parsed from obj (or empty collection on error with a printout)
    */
  def getElements[T](obj: JsValue, path: String)(implicit rds: Reads[T]): Seq[T] = {
    val xs = scala.collection.mutable.ArrayBuffer[T]()
    val status = (obj \ path).validate[List[T]] match {
      case s: JsSuccess[List[T]] =>
        s.get.foreach(xs.append(_))
      case e: JsError =>
        println(s"failure parsing list of tags $e of type for $path")
    }
    xs
  }

}
