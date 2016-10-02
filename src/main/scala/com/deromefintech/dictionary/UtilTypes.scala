package com.deromefintech.dictionary

import fastparse.all._

/**
  * Created by philippederome on 2016-10-01.
  */
trait UtilTypes {
  type TypedFIXTagValue = TypedFIXTag[PValue]
  type TypedFIXTagValues = Seq[TypedFIXTagValue]
  type PTypedFixTag = Parser[TypedFIXTagValue]
  type PMapTypedFixTag = Parser[Map[Int, TypedFIXTagValue]]
  type PFullMessage = Parser[(Map[Int, TypedFIXTagValue], Map[Int, TypedFIXTagValue], Map[Int, TypedFIXTagValue])]
}