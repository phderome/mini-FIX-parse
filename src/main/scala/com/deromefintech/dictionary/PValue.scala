package com.deromefintech.dictionary

/**
  * Created by philippederome on 2016-10-01.
  */
sealed trait PValue
case class PInt(v: Int) extends PValue
case class PString(v: String) extends PValue
case class PChar(v: Char) extends PValue
case class PBoolean(v: Boolean) extends PValue
