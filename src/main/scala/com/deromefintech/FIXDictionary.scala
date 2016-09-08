package com.deromefintech

/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary {

  import fastparse.all._

  def parseOfSeq(seq: Seq[TypedFIXTag], f: TypedFIXTag => P[TypedFIXTag]): P[TypedFIXTag] = {
    val pSeq: Seq[P[TypedFIXTag]] = seq.map(x => f(x))
    pSeq match {
      case (head :: tail) =>
        pSeq.fold[P[TypedFIXTag]](head)((y, x) => P(y | x))
      case (head :: Nil) => head
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

  def buildBlock(tag: PTypedFixTag, atEnd: Boolean = false) = {
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
    val msgBodyAsMap = buildBlock(tagParsers)
    P(headerMap ~ tagSep ~ msgBodyAsMap ~ tagSep ~ controlPair.trailerMap)
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
  val trailerMap = buildBlock(trailerTag, atEnd = true)
  case class ControlTagsParserPair(headerTags: PTypedFixTag, trailerMap: PMapTypedFixTag)
  val controlTags = ControlTagsParserPair(headerTag, trailerMap)

  // Body tags
  val clOrdIDTag = StringFIXTag(11, "ClOrdID")
  val orderIDTag = StringFIXTag(37, "OrderID")
  val ordTypeTag = CharacterFIXTag(40, "OrdType")
  val origClOrdIDTag = StringFIXTag(41, "OrigClOrdID")
  val sideTag = CharacterFIXTag(54, "Side")
  val symbolTag = StringFIXTag(55, "Symbol")

  // MsgID Info
  val newOrderSingleMsgIdName = "D"
  val cancelRequestMsgIdName = "F"
  val cxlReplaceMsgIdName = "G"
  val executionReportMsgIdName = "8"
  val orderCxlRejectMsgIdName = "9"

  // New Order Single
  val newOrderTags = Seq(clOrdIDTag, ordTypeTag, sideTag, symbolTag)
  val fullMsgTypeDAsMap = buildFullMsgP(controlTags, newOrderSingleMsgIdName, newOrderTags)

  // Cancel Request
  val cancelRequestTags = Seq(clOrdIDTag, orderIDTag, origClOrdIDTag, sideTag, symbolTag)
  val fullMsgTypeFAsMap = buildFullMsgP(controlTags, cancelRequestMsgIdName, cancelRequestTags)

  // Cancel Replace Request
  val cancelReplaceTags = Seq(clOrdIDTag, orderIDTag, ordTypeTag, origClOrdIDTag,
    sideTag, symbolTag)
  val fullMsgTypeGAsMap = buildFullMsgP(controlTags, cxlReplaceMsgIdName, cancelReplaceTags)

  // Order Cancel Reject
  val orderCancelRejectTags = Seq(clOrdIDTag, orderIDTag, ordTypeTag, origClOrdIDTag,
    sideTag, symbolTag)
  val fullMsgType9AsMap = buildFullMsgP(controlTags, orderCxlRejectMsgIdName, orderCancelRejectTags)

  // Execution Report
  val executionReportTags = Seq(clOrdIDTag, orderIDTag, ordTypeTag, origClOrdIDTag, sideTag, symbolTag)
  val fullMsgType8AsMap = buildFullMsgP(controlTags, executionReportMsgIdName, executionReportTags)
}

