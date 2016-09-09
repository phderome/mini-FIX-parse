package com.deromefintech

import scala.collection.mutable.ArrayBuffer

/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary {

  import fastparse.all._
  import com.typesafe.config.{ConfigParseOptions,ConfigFactory, ConfigSyntax}
  import play.api.libs.json._
  import play.api.libs.json.Reads._ // Custom validation helpers
  import play.api.libs.functional.syntax._

  def parseOfSeq(seq: Seq[TypedFIXTag], f: TypedFIXTag => P[TypedFIXTag]): Option[P[TypedFIXTag]] = {
    val pSeq: Seq[P[TypedFIXTag]] = seq.map(x => f(x))
    pSeq match {
      case x if x.isEmpty => None
      case z => Some(z.fold[P[TypedFIXTag]](z.head)((y, x) => P(y | x)))
    }

  }

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

  trait FIXTypes {
    val ttype: String
  }

  type PTypedFixTag = Parser[TypedFIXTag]  // possible HACK, can this be parameterized by T and allow sequences to be parsed on different types?
  // Seems not at first.
  type PMapTypedFixTag = Parser[Map[Int, TypedFIXTag]]

  case class StringFIXTag(id: Int, name: String, value: String = "") extends TypedFIXTag {
    override val tagParser: Parser[String] = tagStringValue
    def setValue(s: String) = this.copy(value = s)
  }
  object StringFIXTag extends FIXTypes {
    override val ttype = "String"
  }

  case class CharacterFIXTag(id: Int, name: String, value: Character = Char.MinValue) extends TypedFIXTag {
    override val tagParser: Parser[Character] = tagCharValue
    def setValue(s: String) = this.copy(value = s.charAt(0))
  }
  object CharacterFIXTag extends FIXTypes {
    override val ttype = "Character"
  }

  case class IntegerFIXTag(id: Int, name: String, value: Int = 0) extends TypedFIXTag {
    override val tagParser: Parser[Integer] = tagIntValue
    def setValue(s: String) = this.copy(value = s.toInt)
  }

  object IntegerFIXTag extends FIXTypes {
    override val ttype = "Integer"
  }

  case class BooleanFIXTag(id: Int, name: String, value: Boolean = false) extends TypedFIXTag {
    override val tagParser: Parser[java.lang.Boolean] = tagBooleanValue
    def setValue(s: String) = this.copy(value = s == "Y")
  }

  object BooleanFIXTag extends FIXTypes {
    override val ttype = "java.lang.Boolean"
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

  // headerTag, trailerMap
  def buildFullMsgP(controlPair: ControlTagsParserPair,
                    msgTypeIDTagName: String,
                    tags: Seq[TypedFIXTag]) = {
    val msgTypeIDTag = buildMsgTypeIDTag(msgTypeIDTagName)
    val tagParsers = parseOfSeq(tags, TypedFIXTag.parseBuilder)

    val headerMap = buildHeaderBlock(P(controlPair.headerTags | msgTypeIDTag))
    // arguable design to leave out msgTypeID(35) from regular headerMap and inject only
    // when building the map of tags.
    for {x <- tagParsers
         msgBodyAsMap = buildBlock(x)
    } yield P(headerMap ~ tagSep ~ msgBodyAsMap ~ tagSep ~ controlPair.trailerMap)
  }

  def buildMsgTypeIDTag(value: String, tagName: String = "MsgType", code: Int = 35) =
    P(s"$code=" ~ value).!.map(x => StringFIXTag(code, tagName, value))

  // Header tags
  val possDupeTag = BooleanFIXTag(43, "possDupe")
  val headerTagsSeq = Seq(possDupeTag)
  val headerTag = parseOfSeq(headerTagsSeq, TypedFIXTag.parseBuilder)

  // Trailer tags
  val checkSumTag = StringFIXTag(10, "CheckSum")
  val signatureLengthTag = IntegerFIXTag(93, "SignatureLength") // HACK: should be Length
  val signatureTag = StringFIXTag(89, "Signature") // HACK: should be Data
  val trailerTagsSeq = Seq(checkSumTag, signatureLengthTag, signatureTag)
  val trailerTag = parseOfSeq(trailerTagsSeq, TypedFIXTag.parseBuilder)

  // Trailer
  val trailerMap = trailerTag.map( t => buildBlock(t, atEnd = true))
  case class ControlTagsParserPair(headerTags: PTypedFixTag, trailerMap: PMapTypedFixTag)
  val controlTags = { for {
    h <- headerTag
    t <- trailerMap
  } yield ControlTagsParserPair(h, t) }.get // HACK, we know it's not empty

  // TO DO, Work In Progress, read and parse the data here
  val conf = ConfigFactory.load() // to load tags dynamically from a resource file
  val options = ConfigParseOptions.defaults()
    .setSyntax(ConfigSyntax.JSON)
    .setAllowMissing(false)

  case class ConfigTagInfo(id: Int, name: String)
  implicit val configTagReads: Reads[ConfigTagInfo] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "name").read[String]
    )(ConfigTagInfo.apply _)

  def getTags(obj: JsValue, path: String): Seq[ConfigTagInfo] = {
    var myTags = ArrayBuffer[ConfigTagInfo]()
    val status = (obj \ path).validate[List[ConfigTagInfo]] match {
      case s: JsSuccess[List[ConfigTagInfo]] =>
        s.get.foreach(x => myTags.append(x))
      case e: JsError =>
        println(s"failure parsing list of tags $e of type for $path")
    }
    myTags
  }

  def buildMsgPFromConfig(msgIdConfigName: String) = {
    val configTags = conf.getString(msgIdConfigName)
    val obj = Json.parse(configTags)
    val msgTypeIDTagName: String = (obj \ "msgType").as[String]
    val stringTags = getTags(obj,"stringTags").map(x => StringFIXTag(x.id, x.name))
    val charTags = getTags(obj, "charTags").map(x => CharacterFIXTag(x.id, x.name)) // HACK, should not do this for all types sequentially.
    val bodyTags = stringTags ++ charTags
    buildFullMsgP(controlTags, msgTypeIDTagName, bodyTags).get // HACK (technically could be none, in practice not)
  }

  // New Order Single
  val fullMsgTypeDAsMap = buildMsgPFromConfig("NewOrderTags")
  // Cancel Request
  val fullMsgTypeFAsMap = buildMsgPFromConfig("CancelRequestTags")
  // Cancel Replace Request
  val fullMsgTypeGAsMap = buildMsgPFromConfig("CancelReplaceTags")
  // Order Cancel Reject
  val fullMsgType9AsMap = buildMsgPFromConfig("OrderCancelRejectTags")
  // Execution Report
  val fullMsgType8AsMap = buildMsgPFromConfig("ExecutionReportTags")
}
