package com.deromefintech.dictionary

import fastparse.all._

/**
  * Created by philippederome on 2016-10-01.
  */
trait TypedFIXTag[+T] {
  val id: Int
  val name: String
  val tagParser: Parser[T]
  def setValue(s: String): TypedFIXTag[T]
}

object TypedFIXTag extends UtilTypes {
  def parseBuilder(t: TypedFIXTagValue): PTypedFixTag =
    P(s"${t.id}=" ~ t.tagParser).!.map { s: String => t.setValue(s) }
}