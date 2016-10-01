package com.deromefintech.dictionary

// Acknowledgements to MIT Licence for Li Haoyi's work on FastParse
/*
The MIT License (MIT)

Copyright (c) 2014 Li Haoyi (haoyi.sg@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
  and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/**
  * Created by philippederome on 2016-09-04.
  */
import fastparse.core.Parsed

object FIXApplication {
import FIXDictionary._

  def main(args: Array[String]): Unit = {
    val SOH = 1.toChar // real delimiter in FIX, but for clarity in testing we use commas and replace it with SOH thereafter
    val test3 = tagId.parse("55")
    println(test3)  // 3
    val Parsed.Success(_, 2) = test3

    val test4 = tagId.parse("4b")
    println(test4)
    val Parsed.Success(_, 1) = test4

    val test6 = fullMsgTypeDAsMap.parse("35=D,43=N,54=1,10=100".replace(',', SOH))
    println(s"test6 $test6 ${test6.index}")
    val Parsed.Success(_, 21) = test6

    val test7 = fullMsgTypeDAsMap.parse("35=D,43=Y1,10=100".replace(',', SOH))
    println(s"test7 $test7 ${test7.index}")
    val Parsed.Failure(_, 9, _) = test7

    val test8 = fullMsgTypeDAsMap.parse("35=D,43=T,10=100".replace(',', SOH))
    println(test8) // 8 assert fails as 43 is bad
    val Parsed.Failure(_, 5, _) = test8

    val test9 = fullMsgTypeDAsMap.parse("35=D,43=N,40=11,10=100".replace(',', SOH))
    println(test9) // assert fails as 54 is too long
    val Parsed.Failure(_, 10, _) = test9

    val test10 = fullMsgTypeDAsMap.parse("35=D,43=N,54=212,40=P,10=100".replace(',', SOH))
    println(test10)  // assert fails as 54 is too long
    val Parsed.Failure(_, 10, _) = test10

    val test11 = fullMsgTypeDAsMap.parse("35=D,43=Y,54=0,40=0,55=BMO,10=100".replace(',', SOH))
    println(s"test 11 $test11")
    val Parsed.Success(_, 33) = test11

    val test12 = fullMsgTypeDAsMap.parse("35=D,43=YYY,54=0,40=0,55=BMO,10=100".replace(',', SOH))
    println(s"test 12 $test12 ${test12.index}")  // assert fails as 43 is too long
    val Parsed.Failure(_, 9, _) = test12

    val test13 = fullMsgTypeDAsMap.parse("54=0,43=N,40=0,55=BMO,10=100".replace(',', SOH))
    println(s"test 13 $test13 ${test13.index}")  // assert fails as header is missing
    val Parsed.Failure(_, 0, _) = test13

    val test14 = fullMsgTypeDAsMap.parse("35=D,43=N,54=0,40=0,10=100,55=BMO".replace(',', SOH))
    println(s"test 14 $test14 index:${test14.index}")  // assert fails as there is spurious body tag in trailer
    val Parsed.Failure(_, 26, _) = test14

    val test15 = fullMsgTypeDAsMap.parse("35=D,43=Y,54=0,40=0,37=BAD,55=BMO,10=100".replace(',', SOH))
    println(s"test 15 $test15 ${test15.index}")
    val Parsed.Failure(_, 20, _) = test15  // because of tag 37

    val test16 = fullMsgTypeFAsMap.parse("35=F,43=Y,54=0,37=BAD,55=BMO,10=100".replace(',', SOH))
    println(s"test 16 $test16 ${test16.index}")
    val Parsed.Success(_, 35) = test16  // accepts tag 37 because it's messge F

  }

}

