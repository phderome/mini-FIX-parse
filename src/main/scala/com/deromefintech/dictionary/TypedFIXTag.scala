package com.deromefintech.dictionary

import fastparse.all._

/**
  * Created by philippederome on 2016-10-01.
  */
trait TypedFIXTag {
  val id: Int
  val name: String
  val tagParser: Parser[PValue]
  def setValue(s: String): TypedFIXTag
}

object TypedFIXTag extends UtilTypes {
  def parseBuilder(t: TypedFIXTag): PTypedFixTag =
    P(s"${t.id}=" ~ t.tagParser).!.map { s: String =>
      t.setValue(s)
    }
}