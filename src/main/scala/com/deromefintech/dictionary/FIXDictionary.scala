package com.deromefintech.dictionary

import com.deromefintech.dictionary.TagInfo.PTagInfo
import fastparse.all._

/**
  * Created by philippederome on 2016-09-05.
  */
abstract sealed class FIXType
case object FIXBoolean extends FIXType
case object FIXInt extends FIXType
case object FIXChar extends FIXType

object FIXType {
  def fromString(value: String): Option[FIXType] = {
    List(FIXBoolean, FIXInt, FIXChar).find(_.toString.substring("FIX".length) == value)
    // we represent Boolean, Int, Char in resource file but those are reserved by language so trim header FIX in name.
  }
}

object FIXDictionary extends ConfigJson {
  type TagInfos = Seq[TagInfo]
  type TagId = Int
  type TagIdToMetaData = Map[TagId, BasicMetaData]
  type TagIdToTagInfo = Map[TagId, TagInfo]
  type PTagIdToTagInfo = Parser[TagIdToTagInfo]
  type PMessage = Parser[(TagIdToTagInfo, TagIdToTagInfo, TagIdToTagInfo)]
  case class TagIdWrapper(id: TagId)

  type FIXTagBuilder = (Int, String) => TagInfo
  val defaultFIXTagBuilder: FIXTagBuilder = StringFIXTag( _, _)
  val booleanFIXTagBuilder: FIXTagBuilder = BooleanFIXTag( _, _)
  val intFIXTagBuilder: FIXTagBuilder = IntFIXTag( _, _)
  val charFIXTagBuilder: FIXTagBuilder = CharFIXTag( _, _)

  val FIXTypeToTagInfo = Map[FIXType, FIXTagBuilder](
    FIXBoolean -> booleanFIXTagBuilder,
    FIXInt -> intFIXTagBuilder,
    FIXChar -> charFIXTagBuilder
  ).withDefaultValue(defaultFIXTagBuilder)

  case class MetaData(id: Int, name: String, `type`: String) extends BasicMetaData
  case class ControlTagsParserPair(headerTags: PTagInfo, trailerMap: PTagIdToTagInfo)

  val configDocument = scala.io.Source.fromResource("application.conf").mkString

  val MSG_TYPE_ID = 35
  val TAG_VALUE_SEP = "="
  val SOHAsString = 1.toChar.toString // the FIX tag separator as String

  // define actual Parser objects, starting with basics or building blocks and eventually getting higher-level ones.
  val tagIntValue = P(CharIn('0' to '9').rep(1).!.map(s => PInt(s.toInt)))
  val tagStringValue = P(CharIn(' ' to '~')).rep(1).! // printable character (Ascii 32 to 126).
  // requires lookahead for tag separator
  val tagCharValue = AnyChar.!.map(x => PChar(x.charAt(0))) ~ (End | & (SOHAsString))
  val tagBooleanValue = P("Y" | "N").!.map(s => PBoolean(s == "Y"))
  val tagSep = P(SOHAsString)

  // Move up a bit in abstraction...
  // Examples, we can recognize 35=D with input value=D or 35=F with input value=F (metadata  string MsgType is not essential for processing
  // but could turn out to be useful for consumer of the parser)
  def buildPMsgTypeID(value: String) =
    P( (MSG_TYPE_ID + TAG_VALUE_SEP) ~ value).!.map(x => StringFIXTag(MSG_TYPE_ID, "MsgType").setValue(value))

  // Move further up a bit in abstraction...
  /**
    *
    * @param infos a collection of tagInfos we want to obtain a parser when any but not more of the constituents are valid
    * @param getParser a function that obtains a parser PTagInfo that can parse TagInfo given a TagInfo, one that can recognize the given TagInfo.
    * @return on success a Some, failure a None out of many tags we can parse, we can parse the whole collection as one unit
    *         meaning any constituent may appear (if infos contain tags 35, 40, 54, result will be able to recognize any
    *         of these 3 tags but no other, which is done combining them in fold as P(accum | next).
    */
  def parseTagInfos(infos: TagInfos, getParser: TagInfo => PTagInfo): Option[PTagInfo] =
  infos.map(getParser) match {
    case x if x.isEmpty => None
    case x => Some(x.foldLeft[PTagInfo](x.head)((accum, next) => P(accum | next)))
  }

  def buildTagInfosFromConfig(tagGroup: String): TagInfos = {
    getMetaDatas(configDocument, tagGroup) map { m =>
      FIXType.fromString(m.`type`)
        .map(x => FIXTypeToTagInfo(x)(m.id, m.name))
        .getOrElse(defaultFIXTagBuilder(m.id, m.name))
    }
  }

  // meant to be used for a distinct block of tags within a FIX message, e.g headerTags or trailerTags.
  def buildPTagInfoFromConfig(value: String): PTagInfo = {
    val tags = buildTagInfosFromConfig(value)
    parseTagInfos(tags, TagInfo.parseBuilder).fold[PTagInfo](Fail)(identity)
  }

  /**
    *
    * @param headerTags String representing header tags
    * @param trailerTags String representing trailer tags
    * @return
    * we parse control tags (headerTags and trailerTags) separately from bodyTags because they are constant for each FIX MsgType (D,F,G,8,9)
    * whereas bodyTags depend on MsgType.
    */
  def getControlTags(headerTags: String, trailerTags: String): ControlTagsParserPair =
    ControlTagsParserPair(buildPTagInfoFromConfig(headerTags),
      Some(buildPTagInfoFromConfig(trailerTags)).fold[PTagIdToTagInfo](Fail)(t => buildMapForBlock(t, atEnd = true)))

  // Build from a block of tags a parser that can construct a map of tagId to TagInfo, which is what a client would need
  // This has use for the header, body, and trailer of a FIX message for which independent, separate collections of tags is desirable
  // so that FIX session management can be handled independently from FIX message business logic.
  // given 55=YHOO|54=B|40=P in a body block you get a map (55-> TagInfoFor(YHOO), 54-> TagInfoFor(B), 40-> TagInfoFor(P)), loosely speaking
  def buildMapForBlock(tag: PTagInfo, atEnd: Boolean = false): PTagIdToTagInfo = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock = if (atEnd) P(tag ~ tagTail ~ End) else P(tag ~ tagTail)
    val block = rawBlock.map {
      case (x: TagInfo, y: TagInfos) => y :+ x
    }
    block.map(_.map { y: TagInfo => (y.id, y) }.toMap)
  }

  // actually builds a Parser for a FIX Message of the given msgTypeIDTagName with help of info for controlPair and tagIds
  def buildPMsg(controlPair: ControlTagsParserPair,
                msgTypeValue: Option[String],
                    tagIds: TagInfos): Option[PMessage] =
    // arguable design to leave out msgTypeID(35) from regular headerMap and inject only
    // when building the map of tags.
    for {reconciledTags <- parseTagInfos(tagIds, TagInfo.parseBuilder) // tag Parsers, get reconciled tags from their keys tagIds
         msgTypeID <- msgTypeValue.map(buildPMsgTypeID)
         headerMap = buildMapForBlock(P(controlPair.headerTags | msgTypeID))
         // this will need to be fixed since msgTypeID should be viewed as a predecessor of headerTags as it's mandatory for it by FIX
         // to be the first tag of headerTags. We may as well make a semantic translation and interpret that as saying msgType is not
         // a header but precedes the header.
         msgBodyAsMap = buildMapForBlock(reconciledTags)
    } yield P(headerMap ~ tagSep ~ msgBodyAsMap ~ tagSep ~ controlPair.trailerMap)

  // actually builds a Parser for a FIX Message whose body tags can be obtained from key via configuration
  // with help of controlTags that are constant for any FIX Message of the same FIX Version
  /**
    *
    * @param tagGroup used to identify the tags of a message
    * @param controlTags a pair of header tag info and trailer tag info with trailer info already arranged as a map of TagId to meta data
    * @param bodyTags the map from tagId to the meta data (with default value to be reevaluated) for that tag
    * @return A PMessage, which consists of headerTags, bodyTags, and trailerTags
    * Here we load from resource files which tags belong to each of the 3 sets/sequences
    * of headerTags, bodyTags, and trailerTags, headerTags and trailerTags having been pre-loaded within controlTags.
    * In general, this parsing is actually FIX Version dependent (to be done later), because the contents of the valid tags
    * for these 3 tags depend on FIX Version and msgId (key).
    * So, eventually, there should be a design for each FIX Version to build a set of PMessage parsers
    * for all valid MsgId within that FIX Version.
    */
  def buildPMsgFromConfig(tagGroup: String, controlTags: ControlTagsParserPair,
                          bodyTags: TagIdToTagInfo): PMessage = {
    val (msgTypeValue, msgBodyTagIds) = getMsgTypeAndBodyTagIds(configDocument, tagGroup, bodyTags)
    buildPMsg(controlTags, msgTypeValue, msgBodyTagIds).fold[PMessage](Fail)(identity) // note the default to Fail on None Option.
  }

  val controlTags = getControlTags("headerTags", "trailerTags")
  val bodyTags = buildTagInfosFromConfig("bodyTags").map(t => (t.id, t)).toMap
  // New Order Single
  val FIXMsgDP = buildPMsgFromConfig("NewOrderTags", controlTags, bodyTags)
  // Cancel Request
  val FIXMsgFP = buildPMsgFromConfig("CancelRequestTags", controlTags, bodyTags)
  // Cancel Replace Request
  val FIXMsgGP = buildPMsgFromConfig("CancelReplaceTags", controlTags, bodyTags)
  // Order Cancel Reject
  val FIXMsg9P = buildPMsgFromConfig("CancelRejectTags", controlTags, bodyTags)
  // Execution Report
  val FIXMsg8P = buildPMsgFromConfig("ExecutionReportTags", controlTags, bodyTags)

  case class StringFIXTag(id: Int, name: String, value: PString = PString("")) extends TagInfo {
    override val tagParser: Parser[PString] = tagStringValue.map(x => PString(x))
    def setValue(s: String) = this.copy(value = PString(s))
  }

  case class CharFIXTag(id: Int, name: String, value: PChar = PChar(Char.MinValue)) extends TagInfo {
    override val tagParser: Parser[PChar] = tagCharValue
    def setValue(s: String) = this.copy(value = PChar(s.charAt(0)))
  }

  case class IntFIXTag(id: Int, name: String, value: PInt = PInt(0)) extends TagInfo {
    override val tagParser: Parser[PInt] = tagIntValue
    def setValue(s: String) = this.copy(value = PInt(s.toInt))
  }

  case class BooleanFIXTag(id: Int, name: String, value: PBoolean = PBoolean(false)) extends TagInfo {
    override val tagParser: Parser[PBoolean] = tagBooleanValue
    def setValue(s: String) = this.copy(value = PBoolean(s == "Y"))
  }

}
