package com.deromefintech

import com.typesafe.config.{Config, ConfigFactory}
import fastparse.all._

import scala.collection.mutable.ArrayBuffer
// FastParse claims to be faster than most and hence Play's JSON parser,
// however Play is more established in the broader Scala community.
// Hence we use Play here in place of FastParse for JSON parsing.
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary {

  val intNumber = P(CharIn('0' to '9').rep(1).!.map(s => s.toInt))
  val word = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z')).rep(1).!
  val tagId = intNumber
  val tagStringValue = word
  val SOH = 1.toChar.toString
  // requires lookahead for tag separator
  val tagCharValue = AnyChar.!.map(x => x.charAt(0)) ~ (End | & (SOH))

  val tagBooleanValue = P("Y" | "N").!.map(s =>  s == "Y")
  val tagIntValue = intNumber
  val tagSep = P(SOH)

  trait TypedFIXTag[+T] {
    val id: Int
    val name: String
    val tagParser: Parser[T]
    def setValue(s: String): TypedFIXTag[T]
  }

  object TypedFIXTag {
    def parseBuilder(t: TypedFIXTag[Any]): Parser[TypedFIXTag[Any]] =
      P(s"${t.id}=" ~ t.tagParser).!.map { s: String => t.setValue(s)
      }
  }

  type PTypedFixTag = Parser[TypedFIXTag[Any]]
  type PMapTypedFixTag = Parser[Map[Int, TypedFIXTag[Any]]]
  type PFullMessage = Parser[(Map[Int, TypedFIXTag[Any]], Map[Int, TypedFIXTag[Any]], Map[Int, TypedFIXTag[Any]])]

  // hack Java String does not extend Any, so we "promote" such Strings to StringWrap a case class that does extend Any
  case class StringWrap(value: String)
  case class StringFIXTag(id: Int, name: String, value: StringWrap = StringWrap("")) extends TypedFIXTag[StringWrap] {
    override val tagParser: Parser[StringWrap] = tagStringValue.map(x => StringWrap(x))
    def setValue(s: String) = this.copy(value = StringWrap(s))
  }

  case class CharFIXTag(id: Int, name: String, value: Character = Char.MinValue) extends TypedFIXTag[Char] {
    override val tagParser: Parser[Char] = tagCharValue
    def setValue(s: String) = this.copy(value = s.charAt(0))
  }

  case class IntFIXTag(id: Int, name: String, value: Int = 0) extends TypedFIXTag[Int] {
    override val tagParser: Parser[Int] = tagIntValue
    def setValue(s: String) = this.copy(value = s.toInt)
  }

  case class BooleanFIXTag(id: Int, name: String, value: Boolean = false) extends TypedFIXTag[Boolean] {
    override val tagParser: Parser[Boolean] = tagBooleanValue
    def setValue(s: String) = this.copy(value = s == "Y")
  }

  def buildHeaderBlock(tag: PTypedFixTag, atEnd: Boolean = false) = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock =
      if (atEnd) P(tag ~ tagTail ~ End)
      else P(tag ~ tagTail)

    val block = rawBlock.map {
      case (x: TypedFIXTag[Any], y: Seq[TypedFIXTag[Any]]) => y :+ x
    }
    block.map(
      x => x.map { y: TypedFIXTag[Any] => (y.id, y) }.toMap)
  }

  def buildBlock(tag: PTypedFixTag, atEnd: Boolean = false): PMapTypedFixTag = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock = if (atEnd) P(tag ~ tagTail ~ End) else P(tag ~ tagTail)
    val block = rawBlock.map {
      case (x: TypedFIXTag[Any], y: Seq[TypedFIXTag[Any]]) => y :+ x
    }
    block.map(_.map { y: TypedFIXTag[Any] => (y.id, y) }.toMap)
  }

  case class BasicConfigTagInfo(id: Int)
  // @see https://groups.google.com/forum/?fromgroups=#!starred/play-framework/hGrveOkbJ6U on Reads with single item case classes
  implicit val myTypeRead = (JsPath \ "id").read[Int].map(BasicConfigTagInfo)
  def getBasicTags(obj: JsValue, path: String): Seq[BasicConfigTagInfo] = {
    val myTags = ArrayBuffer[BasicConfigTagInfo]()
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
    val myTags = ArrayBuffer[ConfigTagInfo]()
    val status = (obj \ path).validate[List[ConfigTagInfo]] match {
      case s: JsSuccess[List[ConfigTagInfo]] =>
        s.get.foreach(x => myTags.append(x))
      case e: JsError =>
        println(s"failure parsing list of tags $e of type for $path")
    }
    myTags
  }

  def parseOfSeq(seq: Seq[TypedFIXTag[Any]], f: TypedFIXTag[Any] => P[TypedFIXTag[Any]]): Option[P[TypedFIXTag[Any]]] = {
    val pSeq: Seq[P[TypedFIXTag[Any]]] = seq.map(x => f(x))
    pSeq match {
      case x if x.isEmpty => None
      case z => Some(z.fold[P[TypedFIXTag[Any]]](z.head)((y, x) => P(y | x)))
    }
  }

  def buildMsgTypeIDTag(value: String) =
    P("35=" ~ value).!.map(x => StringFIXTag(35, "MsgType").setValue(value))

  def buildFullMsgP(controlPair: ControlTagsParserPair,
                    msgTypeIDTagName: Option[String],
                    tags: Seq[TypedFIXTag[Any]]) =
    // arguable design to leave out msgTypeID(35) from regular headerMap and inject only
    // when building the map of tags.
    for {x <- parseOfSeq(tags, TypedFIXTag.parseBuilder) // tag Parsers
         msgTypeID <- msgTypeIDTagName.map(buildMsgTypeIDTag)
         headerMap = buildHeaderBlock(P(controlPair.headerTags | msgTypeID))
         msgBodyAsMap = buildBlock(x)
    } yield P(headerMap ~ tagSep ~ msgBodyAsMap ~ tagSep ~ controlPair.trailerMap)

  def buildTagsFromConfig(conf: Config, key: String): Seq[TypedFIXTag[Any]] = {
    val configTags = conf.getString(key)
    val obj = Json.parse(configTags)
    getTags(obj, "tags").map{x: ConfigTagInfo =>
      x.`type` match {
        case "Boolean" => BooleanFIXTag(x.id, x.name): TypedFIXTag[Boolean]
        case "Int" => IntFIXTag(x.id, x.name): TypedFIXTag[Int]
        case "Char" => CharFIXTag(x.id, x.name): TypedFIXTag[Char]
        case _ => StringFIXTag(x.id, x.name): TypedFIXTag[StringWrap]
      }
    }
  }

  def buildPTagsPFromConfig(conf: Config, key: String) = {
    val allTags = buildTagsFromConfig(conf, key)
    parseOfSeq(allTags, TypedFIXTag.parseBuilder).fold[P[TypedFIXTag[Any]]](Fail)(identity)
  }

  case class ControlTagsParserPair(headerTags: PTypedFixTag, trailerMap: PMapTypedFixTag)
  val controlTagsParserPairFail = ControlTagsParserPair(Fail, Fail)
  def getControlTags(conf: Config): ControlTagsParserPair =
    ControlTagsParserPair(buildPTagsPFromConfig(conf, "headerTags"),
      Some(buildPTagsPFromConfig(conf, "trailerTags")).fold[PMapTypedFixTag](Fail)(t => buildBlock(t, atEnd = true)))

  def buildMsgPFromConfig(conf: Config, key: String, controlTags: ControlTagsParserPair,
                          bodyTags: Map[Int, TypedFIXTag[Any]]): PFullMessage = {
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
