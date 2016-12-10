package com.deromefintech.dictionary

import com.deromefintech.dictionary.TagInfo.PTagInfo
import fastparse.all._

/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary extends ConfigJson {
  type TagInfos = Seq[TagInfo]
  type TagId = Int
  type TagIdToMetaData = Map[TagId, BasicMetaData]
  type TagIdToTagInfo = Map[TagId, TagInfo]
  type PTagIdToTagInfo = Parser[TagIdToTagInfo]
  type PMessage = Parser[(TagIdToTagInfo, TagIdToTagInfo, TagIdToTagInfo)]
  case class TagIdWrapper(id: TagId)
  case class MetaData(id: Int, name: String, `type`: String) extends BasicMetaData
  case class ControlTagsParserPair(headerTags: PTagInfo, trailerMap: PTagIdToTagInfo)


  // HACK: types are overly simplified, SendingTime is not a String. Should be back in config file not in code.
  val  confHeaderTags = """{
   "tags":[
   {"id":8, "name":"BeginString", "type":"String"},
   {"id":9, "name": "BodyLength", "type":"Int"},
   {"id":34, "name":"MsgSeqNum", "type": "Int"},
   {"id":35, "name": "MsgType", "type":"String"},
   {"id":43, "name":"possDupe", "type":"Boolean"},
   {"id":49, "name":"SenderCompID", "type":"String"},
   {"id":52, "name":"SendingTime", "type":"String"},
   {"id":56, "name":"TargetCompID", "type":"String"},
   {"id":97, "name":"PossResend", "type":"Boolean"},
   {"id":115, "name":"OnBehalfOfCompID", "type":"String"},
   {"id":128, "name":"DeliverToCompID", "type":"String"}
   ]
    }"""

  // HACK: should be Length for 93 and Data for 89
  val confTrailerTags = """{
   "tags":[
     {"id":10, "name": "CheckSum", "type":"String"},
     {"id":89, "name": "Signature", "type":"String"},
     {"id":93, "name": "SignatureLength", "type":"String"}
   ]
    }"""

  val confBodyTags = """{
   "tags":[
   {"id":11, "name": "ClOrdID", "type":"String"},
   {"id":37, "name": "OrderID", "type":"String"},
   {"id":40, "name": "OrdType", "type":"Char"},
   {"id":41, "name": "OrigClOrdID", "type":"String"},
   {"id":54, "name": "Side", "type":"Char"},
   {"id":55, "name": "Symbol", "type":"String"}
   ]
    }"""

  val confNewOrderTags = """{
   "msgType":"D",
   "tags":[
   {"id":11},
   {"id":55},
   {"id":40},
   {"id":54}
   ]}"""

  val confCancelRequestTags = """{
   "msgType":"F",
   "tags":[
   {"id":11},
   {"id":37},
   {"id":41},
   {"id":54},
   {"id":55}
   ]}"""

  val confCancelReplaceTags = """{
   "msgType":"G",
   "tags":[
   {"id":11},
   {"id":37},
   {"id":41},
   {"id":54},
   {"id":40},
   {"id":55}
   ]}"""

  val confOrderCancelRejectTags = """{
   "msgType":"9",
   "tags":[
   {"id":11},
   {"id":37},
   {"id":41}
   ]}"""

  val confExecutionReportTags = """{
   "msgType":"8",
   "tags":[
   {"id":11},
   {"id":37},
   {"id":41},
   {"id":54},
   {"id":40},
   {"id":55}
   ]}"""

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

  def buildTagInfosFromConfig(value: String): TagInfos = {
    getMetaDatas(value) map { m: MetaData =>
      m.`type` match {
        case "Boolean" => BooleanFIXTag(m.id, m.name)
        case "Int" => IntFIXTag(m.id, m.name)
        case "Char" => CharFIXTag(m.id, m.name)
        case _ => StringFIXTag(m.id, m.name)
      }
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
    * @param value used to identify the tags of a msgType
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
  def buildPMsgFromConfig(value: String, controlTags: ControlTagsParserPair,
                          bodyTags: TagIdToTagInfo): PMessage = {
    val (msgTypeValue, msgBodyTagIds) = getMsgTypeAndBodyTagIds(value, bodyTags)
    buildPMsg(controlTags, msgTypeValue, msgBodyTagIds).fold[PMessage](Fail)(identity) // note the default to Fail on None Option.
  }

  val controlTags = getControlTags(confHeaderTags, confTrailerTags)
  val bodyTags = buildTagInfosFromConfig(confBodyTags).map(t => (t.id, t)).toMap
  // New Order Single
  val FIXMsgDP = buildPMsgFromConfig(confNewOrderTags, controlTags, bodyTags)
  // Cancel Request
  val FIXMsgFP = buildPMsgFromConfig(confCancelRequestTags, controlTags, bodyTags)
  // Cancel Replace Request
  val FIXMsgGP = buildPMsgFromConfig(confCancelReplaceTags, controlTags, bodyTags)
  // Order Cancel Reject
  val FIXMsg9P = buildPMsgFromConfig(confOrderCancelRejectTags, controlTags, bodyTags)
  // Execution Report
  val FIXMsg8P = buildPMsgFromConfig(confExecutionReportTags, controlTags, bodyTags)

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
