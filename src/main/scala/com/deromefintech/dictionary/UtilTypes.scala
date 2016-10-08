package com.deromefintech.dictionary

import fastparse.all._

/**
  * Created by philippederome on 2016-10-01.
  */
trait UtilTypes {
  type TagInfos = Seq[TagInfo]
  type PTagInfo = Parser[TagInfo]
  type TagId = Int
  type TagIdToMetaData = Map[TagId, BasicMetaData]
  type TagIdToTagInfo = Map[TagId, TagInfo]
  type PTagIdToTagInfo = Parser[TagIdToTagInfo]
  type PMessage = Parser[(TagIdToTagInfo, TagIdToTagInfo, TagIdToTagInfo)]
  case class TagIdWrapper(id: TagId)
  case class MetaData(id: Int, name: String, `type`: String) extends BasicMetaData
  case class ControlTagsParserPair(headerTags: PTagInfo, trailerMap: PTagIdToTagInfo)
}