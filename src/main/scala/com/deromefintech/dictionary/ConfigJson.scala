package com.deromefintech.dictionary

import com.deromefintech.dictionary.FIXDictionary.{MetaData, TagIdToTagInfo, TagIdWrapper}
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._

import scala.util.{Left, Right}

// FastParse claims to be faster than most generic JSON parsers,
// however Circe (along with Argonaut) is more established in the broader Scala community and parsing JSON is a specialized task
// that may warrant a library with a hand parser.
// Hence we use Circe for JSON parsing.
// In any event this parsing occurs on start up when initializing data FIXDictionary from external file resources
// and not continuously for client use.

trait ConfigJson {
  def getMetaDatas(jsonString: String): Seq[MetaData] = {
    val doc = parse(jsonString).getOrElse(Json.Null)
    doc.hcursor.downField("tags").as[Seq[MetaData]] match {
      case Left(_) => Nil
      case Right(ms) => ms
    }
  }

  def getMsgTypeAndBodyTagIds(json: String, bodyTags: TagIdToTagInfo): (Option[String], Seq[TagInfo]) = {
    val doc: Json = parse(json).getOrElse(Json.Null)
    val cursor: HCursor = doc.hcursor

    val msgTypeValue = cursor.downField("msgType").as[String].toOption

    val msgBodyTagIds = (cursor.downField("tags").as[Seq[TagIdWrapper]] match {
      case Left(_) => Nil
      case Right(wraps) => wraps
    }).flatMap(t => bodyTags.get(t.id))
    (msgTypeValue, msgBodyTagIds)
  }

}
