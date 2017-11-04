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

trait ConfigJson {
  def eitherSeqToSeq[A, B](d: Either[B, Seq[A]]): Seq[A] = d match {
    case Left(_) => Nil
    case Right(x) => x
  }

  def getMetaDatas(document: String, tagGroup: String): Seq[MetaData] = {
    val doc = parse(document).getOrElse(Json.Null)
    eitherSeqToSeq(doc.hcursor.downField(tagGroup).as[Seq[MetaData]])
  }

  def getMsgTypeAndBodyTagIds(document: String, tagGroup: String, bodyTags: TagIdToTagInfo): (Option[String], Seq[TagInfo]) = {
    val doc = parse(document).getOrElse(Json.Null)
    val cursor = doc.hcursor.downField(tagGroup)

    val msgTypeValue = cursor.downField("msgType").as[String].toOption
    val msgBodyTagIds = eitherSeqToSeq(cursor.downField("tags").as[Seq[TagIdWrapper]])
      .flatMap(t => bodyTags.get(t.id))
    (msgTypeValue, msgBodyTagIds)
  }
}
