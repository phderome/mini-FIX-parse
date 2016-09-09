package com.deromefintech

import scala.collection.mutable.ArrayBuffer
import fastparse.all._
import com.typesafe.config.{ConfigFactory, Config}
// FastParse claims to be faster than most and hence Play's JSON parser, however Play is more established in the broader Scala community.
// Hence we use Play here in place of FastParse for JSON parsing.
import play.api.libs.json._
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._
/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary {

  val intNumber = P(CharIn('0' to '9').rep(1).!.map(s => new Integer(s.toInt)))   // HACK, Java Object type
  val word = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z')).rep(1).!
  val tagId = intNumber
  val tagStringValue = word
  // requires lookahead for tag separator
  val tagCharValue = AnyChar.!.map(x => new Character(x.charAt(0))) ~ (End | &(",")) // HACK, Java Object type

  val tagBooleanValue = P("Y" | "N").!.map(s => new java.lang.Boolean(s == "Y")) // HACK, Java Object type
  val tagIntValue = intNumber
  val tagSep = P(",") // HACK: in reality, should be SOH 0x01 ASCII

  trait TypedFIXTag {
    val id: Int
    val name: String
    val tagParser: Parser[Object] // Big HACK, somehow should be able to use native types, a type parameter leads to some difficulties for now...
    def setValue(s: String): TypedFIXTag
  }

  object TypedFIXTag {
    def parseBuilder(t: TypedFIXTag): Parser[TypedFIXTag] =
      P(s"${t.id}=" ~ t.tagParser).!.map { s: String => t.setValue(s)
      }
  }

  type PTypedFixTag = Parser[TypedFIXTag]  // possible HACK, can this be parameterized by T and allow sequences to be parsed on different types?
  // Seems not at first.
  type PMapTypedFixTag = Parser[Map[Int, TypedFIXTag]]
  type PFullMessage = Parser[(Map[Int, TypedFIXTag], Map[Int, TypedFIXTag], Map[Int, TypedFIXTag])]

  case class StringFIXTag(id: Int, name: String, value: String = "") extends TypedFIXTag {
    override val tagParser: Parser[String] = tagStringValue
    def setValue(s: String) = this.copy(value = s)
  }

  case class CharacterFIXTag(id: Int, name: String, value: Character = Char.MinValue) extends TypedFIXTag {
    override val tagParser: Parser[Character] = tagCharValue
    def setValue(s: String) = this.copy(value = s.charAt(0))
  }

  case class IntegerFIXTag(id: Int, name: String, value: Int = 0) extends TypedFIXTag {
    override val tagParser: Parser[Integer] = tagIntValue
    def setValue(s: String) = this.copy(value = s.toInt)
  }

  case class BooleanFIXTag(id: Int, name: String, value: Boolean = false) extends TypedFIXTag {
    override val tagParser: Parser[java.lang.Boolean] = tagBooleanValue
    def setValue(s: String) = this.copy(value = s == "Y")
  }

  def buildHeaderBlock(tag: PTypedFixTag, atEnd: Boolean = false) = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock =
      if (atEnd) P(tag ~ tagTail ~ End)
      else P(tag ~ tagTail)

    val block = rawBlock.map {
      case (x: TypedFIXTag, y: Seq[TypedFIXTag]) => y :+ x
    }
    block.map(
      x => x.map { y: TypedFIXTag => (y.id, y) }.toMap)
  }

  def buildBlock(tag: PTypedFixTag, atEnd: Boolean = false): PMapTypedFixTag = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock = if (atEnd) P(tag ~ tagTail ~ End) else P(tag ~ tagTail)
    val block = rawBlock.map {
      case (x: TypedFIXTag, y: Seq[TypedFIXTag]) => y :+ x
    }
    block.map(_.map { y: TypedFIXTag => (y.id, y) }.toMap)
  }

  case class ConfigTagInfo(id: Int, name: String)
  implicit val configTagReads: Reads[ConfigTagInfo] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "name").read[String]
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

  def parseOfSeq(seq: Seq[TypedFIXTag], f: TypedFIXTag => P[TypedFIXTag]): Option[P[TypedFIXTag]] = {
    val pSeq: Seq[P[TypedFIXTag]] = seq.map(x => f(x))
    pSeq match {
      case x if x.isEmpty => None
      case z => Some(z.fold[P[TypedFIXTag]](z.head)((y, x) => P(y | x)))
    }
  }

  def buildMsgTypeIDTag(value: String) =
    P("35=" ~ value).!.map(x => StringFIXTag(35, "MsgType").setValue(value))

  def buildFullMsgP(controlPair: ControlTagsParserPair,
                    msgTypeIDTagName: Option[String],
                    tags: Seq[TypedFIXTag]) =
    // arguable design to leave out msgTypeID(35) from regular headerMap and inject only
    // when building the map of tags.
    for {x <- parseOfSeq(tags, TypedFIXTag.parseBuilder) // tag Parsers
         msgTypeID <- msgTypeIDTagName.map(buildMsgTypeIDTag)
         headerMap = buildHeaderBlock(P(controlPair.headerTags | msgTypeID))
         msgBodyAsMap = buildBlock(x)
    } yield P(headerMap ~ tagSep ~ msgBodyAsMap ~ tagSep ~ controlPair.trailerMap)

  def buildTagsFromConfig(conf: Config, key: String) = {
    val configTags = conf.getString(key)
    val obj = Json.parse(configTags)
    val booleanTags = getTags(obj,"booleanTags").map(x => BooleanFIXTag(x.id, x.name))
    val stringTags = getTags(obj,"stringTags").map(x => StringFIXTag(x.id, x.name))
    val charTags = getTags(obj, "charTags").map(x => CharacterFIXTag(x.id, x.name)) // HACK, should not do this for all types sequentially.
    booleanTags ++ stringTags ++ charTags
  }

  def buildPTagsPFromConfig(conf: Config, key: String) = {
    val allTags = buildTagsFromConfig(conf, key)
    parseOfSeq(allTags, TypedFIXTag.parseBuilder).fold[P[TypedFIXTag]](Fail)(identity)
  }

  case class ControlTagsParserPair(headerTags: PTypedFixTag, trailerMap: PMapTypedFixTag)
  val controlTagsParserPairFail = ControlTagsParserPair(Fail, Fail)
  def getControlTags(conf: Config): ControlTagsParserPair =
    ControlTagsParserPair(buildPTagsPFromConfig(conf, "headerTags"),
      Some(buildPTagsPFromConfig(conf, "trailerTags")).fold[PMapTypedFixTag](Fail)(t => buildBlock(t, atEnd = true)))

  def buildMsgPFromConfig(conf: Config, key: String, controlTags: ControlTagsParserPair, bodyTags: Map[Int, TypedFIXTag]): PFullMessage = {
    val configTags = conf.getString(key)
    val obj = Json.parse(configTags)
    val msgTypeIDTagName = (obj \ "msgType").asOpt[String]
    val msgBodyTags = getTags(obj, "tags").flatMap(t => bodyTags.get(t.id))
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
