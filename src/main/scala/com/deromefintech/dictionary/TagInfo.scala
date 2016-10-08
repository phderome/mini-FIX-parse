package com.deromefintech.dictionary

import fastparse.all._

/**
  * Created by philippederome on 2016-10-01.
  */
trait BasicMetaData {
  val id: Int
  val name: String
}

trait TagInfo extends BasicMetaData {
  val value: PValue
  val tagParser: Parser[PValue]
  def setValue(s: String): TagInfo
}

object TagInfo {
  type PTagInfo = Parser[TagInfo]
  def parseBuilder(t: TagInfo): PTagInfo =
    P(s"${t.id}=" ~ t.tagParser).!.map { s: String =>
      t.setValue(s)
    }
}