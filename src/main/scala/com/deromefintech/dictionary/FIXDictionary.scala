package com.deromefintech.dictionary

import com.deromefintech.dictionary.TagInfo.PTagInfo
import com.typesafe.config.{Config, ConfigFactory}
import fastparse.all._
// FastParse claims to be faster than most generic JSON parsers,
// however Play is more established in the broader Scala community and parsing JSON is a specialized task
// that may warrant a library with a hand parser.
// Hence we use Play here in place of FastParse (or Typelevel's Circe) for JSON parsing.
// In any event the Play parsing occurs on start up when initializing data FIXDictionary from external file resources
// and not continuously for client use.
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, JsValue, Json}

/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary extends PlayJSReads {
  type TagInfos = Seq[TagInfo]
  type TagId = Int
  type TagIdToMetaData = Map[TagId, BasicMetaData]
  type TagIdToTagInfo = Map[TagId, TagInfo]
  type PTagIdToTagInfo = Parser[TagIdToTagInfo]
  type PMessage = Parser[(TagIdToTagInfo, TagIdToTagInfo, TagIdToTagInfo)]
  case class TagIdWrapper(id: TagId)
  case class MetaData(id: Int, name: String, `type`: String) extends BasicMetaData
  case class ControlTagsParserPair(headerTags: PTagInfo, trailerMap: PTagIdToTagInfo)

  val MSG_TYPE_ID = 35
  val TAG_VALUE_SEP = "="
  val SOHAsString = 1.toChar.toString // the FIX tag separator as String

  // Some Reads that can be used for getElements usage
  val configTagReads: Reads[MetaData] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "name").read[String] and
      (JsPath \ "type").read[String]
    )(MetaData.apply _)
  // @see https://groups.google.com/forum/?fromgroups=#!starred/play-framework/hGrveOkbJ6U on Reads with single item case classes
  // (requires a TagIdWrapper here)
  val tagIdReads = (JsPath \ "id").read[Int].map(TagIdWrapper)

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

  def buildTagInfosFromConfig(conf: Config, key: String): TagInfos = {
    def getMetaDatas(obj: JsValue, path: String): Seq[MetaData] =
      getElements(obj, path)(configTagReads)

    val configTags = conf.getString(key)
    val obj = Json.parse(configTags)
    getMetaDatas(obj, "tags").map{x: MetaData =>
      x.`type` match {
        case "Boolean" => BooleanFIXTag(x.id, x.name)
        case "Int" => IntFIXTag(x.id, x.name)
        case "Char" => CharFIXTag(x.id, x.name)
        case _ => StringFIXTag(x.id, x.name)
      }
    }
  }

  // meant to be used for a distinct block of tags within a FIX message, e.g headerTags or trailerTags.
  def buildPTagInfoFromConfig(conf: Config, key: String): PTagInfo = {
    val tags = buildTagInfosFromConfig(conf, key)
    parseTagInfos(tags, TagInfo.parseBuilder).fold[PTagInfo](Fail)(identity)
  }

  /**
    *
    * @param conf a Config object allowing us to read config data from resource files, which we need to identify header and trailer tags
    * @return
    * we parse control tags (headerTags and trailerTags) separately from bodyTags because they are constant for each FIX MsgType (D,F,G,8,9)
    * whereas bodyTags depend on MsgType.
    */
  def getControlTags(conf: Config): ControlTagsParserPair =
    ControlTagsParserPair(buildPTagInfoFromConfig(conf, "headerTags"),
      Some(buildPTagInfoFromConfig(conf, "trailerTags")).fold[PTagIdToTagInfo](Fail)(t => buildMapForBlock(t, atEnd = true)))

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
                    msgTypeIDTagName: Option[String],
                    tagIds: TagInfos): Option[PMessage] =
    // arguable design to leave out msgTypeID(35) from regular headerMap and inject only
    // when building the map of tags.
    for {reconciledTags <- parseTagInfos(tagIds, TagInfo.parseBuilder) // tag Parsers, get reconciled tags from their keys tagIds
         msgTypeID <- msgTypeIDTagName.map(buildPMsgTypeID)
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
    * @param conf instance of Config
    * @param key a key for config so we can identify the tags of a msgType that is 1-1 with the key
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
  def buildPMsgFromConfig(conf: Config, key: String, controlTags: ControlTagsParserPair,
                          bodyTags: TagIdToTagInfo): PMessage = {
    def getTagIds(obj: JsValue, path: String): Seq[TagIdWrapper] =
      getElements(obj, path)(tagIdReads)

    val configTags = conf.getString(key)
    val jsObj = Json.parse(configTags)
    val msgTypeIDTagName = (jsObj \ "msgType").asOpt[String] // don't try to consume exact value yet, be happy to capture an Option.
    val msgBodyTagIds = getTagIds(jsObj, "tags").flatMap(t => bodyTags.get(t.id))
    buildPMsg(controlTags, msgTypeIDTagName, msgBodyTagIds).fold[PMessage](Fail)(identity) // note the default to Fail on None Option.
  }

  val conf = ConfigFactory.load()
  val controlTags = getControlTags(conf)
  val bodyTags = buildTagInfosFromConfig(conf, "bodyTags").map(t => (t.id, t)).toMap
  // New Order Single
  val FIXMsgDP = buildPMsgFromConfig(conf, "NewOrderTags", controlTags, bodyTags)
  // Cancel Request
  val FIXMsgFP = buildPMsgFromConfig(conf, "CancelRequestTags", controlTags, bodyTags)
  // Cancel Replace Request
  val FIXMsgGP = buildPMsgFromConfig(conf, "CancelReplaceTags", controlTags, bodyTags)
  // Order Cancel Reject
  val FIXMsg9P = buildPMsgFromConfig(conf, "OrderCancelRejectTags", controlTags, bodyTags)
  // Execution Report
  val FIXMsg8P = buildPMsgFromConfig(conf, "ExecutionReportTags", controlTags, bodyTags)

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
