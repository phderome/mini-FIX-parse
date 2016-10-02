package com.deromefintech.dictionary

import com.typesafe.config.{Config, ConfigFactory}
import fastparse.all._

// FastParse claims to be faster than most and hence Play's JSON parser,
// however Play is more established in the broader Scala community.
// Hence we use Play here in place of FastParse for JSON parsing.
// In any event the Play parsing occurs on start up when initializing data FIXDictionary from external file resources
// and not continuously for client use.
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary extends UtilTypes {
  val intNumber = P(CharIn('0' to '9').rep(1).!.map(s => PInt(s.toInt)))
  val word = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z')).rep(1).!
  val tagId = intNumber
  val tagStringValue = word
  val SOH = 1.toChar.toString
  // requires lookahead for tag separator
  val tagCharValue = AnyChar.!.map(x => PChar(x.charAt(0))) ~ (End | & (SOH))

  val tagBooleanValue = P("Y" | "N").!.map(s => PBoolean(s == "Y"))
  val tagIntValue = intNumber
  val tagSep = P(SOH)

  case class StringFIXTag(id: Int, name: String, value: PString = PString("")) extends TypedFIXTag[PString] {
    override val tagParser: Parser[PString] = tagStringValue.map(x => PString(x))
    def setValue(s: String) = this.copy(value = PString(s))
  }

  case class CharFIXTag(id: Int, name: String, value: PChar = PChar(Char.MinValue)) extends TypedFIXTag[PChar] {
    override val tagParser: Parser[PChar] = tagCharValue
    def setValue(s: String) = this.copy(value = PChar(s.charAt(0)))
  }

  case class IntFIXTag(id: Int, name: String, value: PInt = PInt(0)) extends TypedFIXTag[PInt] {
    override val tagParser: Parser[PInt] = tagIntValue
    def setValue(s: String) = this.copy(value = PInt(s.toInt))
  }

  case class BooleanFIXTag(id: Int, name: String, value: PBoolean = PBoolean(false)) extends TypedFIXTag[PBoolean] {
    override val tagParser: Parser[PBoolean] = tagBooleanValue
    def setValue(s: String) = this.copy(value = PBoolean(s == "Y"))
  }

  def buildHeaderBlock(tag: PTypedFixTag, atEnd: Boolean = false) = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock =
      if (atEnd) P(tag ~ tagTail ~ End)
      else P(tag ~ tagTail)

    val block = rawBlock.map {
      case (x: TypedFIXTagValue, y: TypedFIXTagValues) => y :+ x
    }
    block.map(
      x => x.map { y: TypedFIXTagValue => (y.id, y) }.toMap)
  }

  def buildBlock(tag: PTypedFixTag, atEnd: Boolean = false): PMapTypedFixTag = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock = if (atEnd) P(tag ~ tagTail ~ End) else P(tag ~ tagTail)
    val block = rawBlock.map {
      case (x: TypedFIXTagValue, y: TypedFIXTagValues) => y :+ x
    }
    block.map(_.map { y: TypedFIXTagValue => (y.id, y) }.toMap)
  }

  case class BasicConfigTagInfo(id: Int)
  // @see https://groups.google.com/forum/?fromgroups=#!starred/play-framework/hGrveOkbJ6U on Reads with single item case classes
  implicit val myTypeRead = (JsPath \ "id").read[Int].map(BasicConfigTagInfo)
  def getBasicTags(obj: JsValue, path: String): Seq[BasicConfigTagInfo] = {
    val myTags = scala.collection.mutable.ArrayBuffer[BasicConfigTagInfo]()
    val status = (obj \ path).validate[List[BasicConfigTagInfo]] match {
      case s: JsSuccess[List[BasicConfigTagInfo]] =>
        s.get.foreach(x => myTags.append(x))
      case e: JsError =>
        println(s"failure parsing list of tags $e of type for $path")
    }
    myTags
  }

  case class ConfigTagInfo(id: Int, name: String, `type`: String)
  implicit val configTagReads: Reads[ConfigTagInfo] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "name").read[String] and
        (JsPath \ "type").read[String]
    )(ConfigTagInfo.apply _)

  def getTags(obj: JsValue, path: String): Seq[ConfigTagInfo] = {
    val myTags = scala.collection.mutable.ArrayBuffer[ConfigTagInfo]()
    val status = (obj \ path).validate[List[ConfigTagInfo]] match {
      case s: JsSuccess[List[ConfigTagInfo]] =>
        s.get.foreach(x => myTags.append(x))
      case e: JsError =>
        println(s"failure parsing list of tags $e of type for $path")
    }
    myTags
  }

  def parseOfSeq(seq: TypedFIXTagValues, f: TypedFIXTagValue => PTypedFixTag): Option[PTypedFixTag] = {
    val pSeq: Seq[PTypedFixTag] = seq.map(x => f(x))
    pSeq match {
      case x if x.isEmpty => None
      case z => Some(z.fold[PTypedFixTag](z.head)((y, x) => P(y | x)))
    }
  }

  def buildMsgTypeIDTag(value: String) =
    P("35=" ~ value).!.map(x => StringFIXTag(35, "MsgType").setValue(value))

  def buildFullMsgP(controlPair: ControlTagsParserPair,
                    msgTypeIDTagName: Option[String],
                    tags: TypedFIXTagValues) =
    // arguable design to leave out msgTypeID(35) from regular headerMap and inject only
    // when building the map of tags.
    for {x <- parseOfSeq(tags, TypedFIXTag.parseBuilder) // tag Parsers
         msgTypeID <- msgTypeIDTagName.map(buildMsgTypeIDTag)
         headerMap = buildHeaderBlock(P(controlPair.headerTags | msgTypeID))
         msgBodyAsMap = buildBlock(x)
    } yield P(headerMap ~ tagSep ~ msgBodyAsMap ~ tagSep ~ controlPair.trailerMap)

  def buildTagsFromConfig(conf: Config, key: String): TypedFIXTagValues = {
    val configTags = conf.getString(key)
    val obj = Json.parse(configTags)
    getTags(obj, "tags").map{x: ConfigTagInfo =>
      x.`type` match {
        case "Boolean" => BooleanFIXTag(x.id, x.name): TypedFIXTag[PBoolean]
        case "Int" => IntFIXTag(x.id, x.name): TypedFIXTag[PInt]
        case "Char" => CharFIXTag(x.id, x.name): TypedFIXTag[PChar]
        case _ => StringFIXTag(x.id, x.name): TypedFIXTag[PString]
      }
    }
  }

  def buildPTagsPFromConfig(conf: Config, key: String) = {
    val allTags = buildTagsFromConfig(conf, key)
    parseOfSeq(allTags, TypedFIXTag.parseBuilder).fold[PTypedFixTag](Fail)(identity)
  }

  case class ControlTagsParserPair(headerTags: PTypedFixTag, trailerMap: PMapTypedFixTag)
  val controlTagsParserPairFail = ControlTagsParserPair(Fail, Fail)
  def getControlTags(conf: Config): ControlTagsParserPair =
    ControlTagsParserPair(buildPTagsPFromConfig(conf, "headerTags"),
      Some(buildPTagsPFromConfig(conf, "trailerTags")).fold[PMapTypedFixTag](Fail)(t => buildBlock(t, atEnd = true)))

  def buildMsgPFromConfig(conf: Config, key: String, controlTags: ControlTagsParserPair,
                          bodyTags: Map[Int, TypedFIXTagValue]): PFullMessage = {
    val configTags = conf.getString(key)
    val obj = Json.parse(configTags)
    val msgTypeIDTagName = (obj \ "msgType").asOpt[String]
    val msgBodyTags = getBasicTags(obj, "tags").flatMap(t => bodyTags.get(t.id))
    buildFullMsgP(controlTags, msgTypeIDTagName, msgBodyTags).fold[PFullMessage](Fail)(identity)
  }

  val conf = ConfigFactory.load()
  val controlTags = getControlTags(conf)
  val bodyTags = buildTagsFromConfig(conf, "bodyTags").map(t => (t.id, t)).toMap
  // New Order Single
  val fullMsgTypeDAsMap = buildMsgPFromConfig(conf, "NewOrderTags", controlTags, bodyTags)
  // Cancel Request
  val fullMsgTypeFAsMap = buildMsgPFromConfig(conf, "CancelRequestTags", controlTags, bodyTags)
  // Cancel Replace Request
  val fullMsgTypeGAsMap = buildMsgPFromConfig(conf, "CancelReplaceTags", controlTags, bodyTags)
  // Order Cancel Reject
  val fullMsgType9AsMap = buildMsgPFromConfig(conf, "OrderCancelRejectTags", controlTags, bodyTags)
  // Execution Report
  val fullMsgType8AsMap = buildMsgPFromConfig(conf, "ExecutionReportTags", controlTags, bodyTags)
}
