package com.deromefintech.dictionary

import FIXDictionary._
import fastparse.core.Parsed
/**
  * Created by philippederome on 2016-10-03.
  */

class FIXDictionaryTest extends UnitTest {

  def checkResult[T](P: Parsed[T], index: Int, isSuccess: Boolean) = {
    P.index should equal(index)
    // matched parameters below are not used, they serve a documentation purpose of what Success and Failure look like
    if (isSuccess) {
      val Parsed.Success(parsed, idxP) = P
    }
    else {
      val Parsed.Failure(lastParser, idxF, extra) = P
    }
  }

  // we construct pseudo FIX messages with commas as separators for legibility but need to update to SOH for real.
  def correctTagSeparator(data: String) = data.replace(',', SOH)

  behavior of "tagId" // a sequence of digits with a FIX message consisting of tagId=value assignments separated by SOH and a few more details
  it should "return correct success index consuming all" in {
    val data = "55"
    val P = tagId.parse(data)
    checkResult(P, data.length, isSuccess = true)
  }
  it should "return correct success index not consuming all" in {
    val data = "4b"
    val P = tagId.parse("4b")
    checkResult(P, "4".length, isSuccess = true) // this yields 4b=value to be invalid in a larger FIX context, because index 1 < 2 the size of 4b
  }

  behavior of "good subset of Msg F 4.2" // most tests are msg D, here we do positive test on another one (F)
  it should "return correct index at end of expression consuming all" in {
    // accepts tag 37 because it's message F
    val data = correctTagSeparator("8=FIX.4.2,35=F,43=Y,54=0,37=BAD,55=BMO,10=100")
    val P = fullMsgTypeFAsMap.parse(data)
    checkResult(P, data.length, isSuccess = true)
  }

  behavior of "good subset of Msg D 4.2"
  it should "return correct index at end of expression consuming all #1" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=N,54=1,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P, data.length, isSuccess = true)
  }

  it should "return correct index at end of expression consuming all #2" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=Y,54=0,40=0,55=BMO,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P, data.length, isSuccess = true)
  }

  behavior of "bad subset of Msg D 4.2"
  it should "return correct index at point of failure for non Boolean tag(43), bad suffix" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=Y1,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P, "8=FIX.4.2,35=D,43=Y".length, isSuccess = false)
  }

  it should "return correct index at point of failure for non Boolean tag(43), immediately bad" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=T,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P,  "8=FIX.4.2,35=D,".length, isSuccess = false)
  }

  behavior of "too long tag for Boolean (should be size 1) of Msg D 4.2"
  it should "return correct index at point of failure for non Boolean tag(43)" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=YYY,54=0,40=0,55=BMO,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P, "8=FIX.4.2,35=D,43=Y".length, isSuccess = false)
  }

  behavior of "too long tag for Char (should be size 1) of Msg D 4.2"
  it should "return correct index at point of failure for non Char tag(40)" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=N,40=11,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P,  "8=FIX.4.2,35=D,43=N,".length, isSuccess = false)
  }

  it should "return correct index at point of failure for non Char tag(54)" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=N,54=212,40=P,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P,  "8=FIX.4.2,35=D,43=N,".length, isSuccess = false)
  }

  behavior of "invalid tag in Msg D 4.2"
  it should "return correct index at point of failure for invalid tag 37" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=Y,54=0,40=0,37=BAD,55=BMO,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P, "8=FIX.4.2,35=D,43=Y,54=0,40=0,".length, isSuccess = false)
  }

  behavior of "missing header"
  it should "return correct index(0) at point of failure for missing header" in {
    val data = correctTagSeparator("54=0,43=N,40=0,55=BMO,10=100")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P, 0, isSuccess = false)
  }

  behavior of "body tag in trailer of Msg D 4.2 (only trailer tags expected)"
  it should "return correct index at point of failure for invalid tag 55" in {
    val data = correctTagSeparator("8=FIX.4.2,35=D,43=N,54=0,40=0,10=100,55=BMO")
    val P = fullMsgTypeDAsMap.parse(data)
    checkResult(P, "8=FIX.4.2,35=D,43=N,54=0,40=0,10=100".length, isSuccess = false)
  }

}
