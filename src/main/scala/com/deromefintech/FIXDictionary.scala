package com.deromefintech

/**
  * Created by philippederome on 2016-09-05.
  */
object FIXDictionary {

  import fastparse.all._

  val intNumber = P(CharIn('0' to '9').rep(1).!.map(_.toInt))
  val word = P(CharIn('0' to '9', 'a' to 'z', 'A' to 'Z')).rep(1).!
  val tagId = intNumber
  val tagStringValue = word
  // requires lookahead for tag separator
  val tagCharValue = AnyChar.!.map(x => x.charAt(0)) ~ (End | &(","))

  val tagBooleanValue = P("Y" | "N").!.map(_ == "Y")
  val tagIntValue = intNumber
  val tagSep = P(",") // in reality, should be SOH 0x01 ASCII

  trait TypedFIXTag {
    val id: Int
    val name: String
  }

  trait FIXTypes {
    val ttype: String
  }

  case class StringFIXTag(id: Int, name: String, val value: String) extends TypedFIXTag

  object StringFIXTag extends FIXTypes {
    override val ttype = "String"

    def parseBuilder(id: Int, name: String) =
      P(s"${id}=" ~ tagStringValue).map(StringFIXTag(id, name, _))
  }

  case class CharFIXTag(id: Int, name: String, value: Char) extends TypedFIXTag

  object CharFIXTag extends FIXTypes {
    override val ttype = "Char"

    def parseBuilder(id: Int, name: String) = {
      val x = P(s"${id}=" ~ tagCharValue)
      x.map(s => CharFIXTag(id, name, s))
    }
  }

  case class IntFIXTag(id: Int, name: String, value: Int) extends TypedFIXTag

  object IntFIXTag extends FIXTypes {
    override val ttype = "Int"

    def parseBuilder(id: Int, name: String) =
      P(s"${id}=" ~ tagIntValue).map(IntFIXTag(id, name, _))
  }

  case class BooleanFIXTag(id: Int, name: String, value: Boolean) extends TypedFIXTag

  object BooleanFIXTag extends FIXTypes {
    override val ttype = "Boolean"

    def parseBuilder(id: Int, name: String) =
      P(s"${id}=" ~ tagBooleanValue).map(BooleanFIXTag(id, name, _))
  }

  def buildBlock(tag: Parser[TypedFIXTag], atEnd: Boolean = false) = {
    val tagTail = P(tagSep ~ tag).rep
    val rawBlock = if (atEnd) P(tag ~ tagTail ~ End)
      else P(tag ~ tagTail)
    val block = rawBlock.map {
      case (x: TypedFIXTag, y: Seq[TypedFIXTag]) => y :+ x
    }
    block.map(
      x => x.map{y: TypedFIXTag => (y.id, y) }.toMap)
  }

  def buildFullMsgP(headerMap: Parser[Map[Int, TypedFIXTag]],
                    trailerMap: Parser[Map[Int, TypedFIXTag]],
                    tag: Parser[TypedFIXTag]) = {
    val msgBodyAsMap = buildBlock(tag)
    P(headerMap ~ tagSep ~ msgBodyAsMap ~ tagSep ~ trailerMap)
  }

  // Header tags
  val msgTypeTag = StringFIXTag.parseBuilder(35, "MsgType")
  val possDupeTag = BooleanFIXTag.parseBuilder(43, "possDupe")
  val headerTag = P(msgTypeTag | possDupeTag )

  // Trailer tags
  val checkSumTag = StringFIXTag.parseBuilder(10, "CheckSum")
  val signatureLengthTag = IntFIXTag.parseBuilder(93, "SignatureLength")  // HACK: should be Length
  val signatureTag = StringFIXTag.parseBuilder(89, "Signature")  // HACK: should be Data
  val trailerTag = P(checkSumTag | signatureLengthTag | signatureTag )

  // Header
  val headerMap = buildBlock(headerTag)

  // Trailer
  val trailerMap = buildBlock(trailerTag, true)

  // Body tags
  val clOrdIDTag = StringFIXTag.parseBuilder(11, "ClOrdID")
  val orderIDTag = StringFIXTag.parseBuilder(37, "OrderID")
  val ordTypeTag = CharFIXTag.parseBuilder(40, "OrdType")
  val origClOrdIDTag = StringFIXTag.parseBuilder(41, "OrigClOrdID")
  val sideTag = CharFIXTag.parseBuilder(54, "Side")
  val symbolTag = StringFIXTag.parseBuilder(55, "Symbol")

  // New Order Single
  val msgTypeDTag = P(clOrdIDTag | ordTypeTag | sideTag | symbolTag)
  val fullMsgTypeDAsMap = buildFullMsgP(headerMap, trailerMap, msgTypeDTag)

  // Cancel Request
  val msgTypeFTag = P(clOrdIDTag | orderIDTag | origClOrdIDTag | sideTag | symbolTag)
  val fullMsgTypeFAsMap = buildFullMsgP(headerMap, trailerMap, msgTypeFTag)

  // Cancel Replace Request
  val msgTypeGTag = P(clOrdIDTag | orderIDTag | ordTypeTag | origClOrdIDTag |
    sideTag | symbolTag)
  val fullMsgTypeGAsMap = buildFullMsgP(headerMap, trailerMap, msgTypeGTag)

  // Execution Report
  val msgType8Tag = P(clOrdIDTag | orderIDTag | ordTypeTag | origClOrdIDTag  |
    sideTag | symbolTag)
  val fullMsgType8AsMap = buildFullMsgP(headerMap, trailerMap, msgType8Tag)

}
