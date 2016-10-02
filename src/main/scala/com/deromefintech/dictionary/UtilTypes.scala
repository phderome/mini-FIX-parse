package com.deromefintech.dictionary

import fastparse.all._

/**
  * Created by philippederome on 2016-10-01.
  */
trait UtilTypes {
  type TypedFIXTagValues = Seq[TypedFIXTag]
  type PTypedFixTag = Parser[TypedFIXTag]
  type PMapTypedFixTag = Parser[Map[Int, TypedFIXTag]]
  type PFullMessage = Parser[(Map[Int, TypedFIXTag], Map[Int, TypedFIXTag], Map[Int, TypedFIXTag])]
}