# mini-FIX-parse
This project explores the capabilities of FastParse (`http://www.lihaoyi.com/fastparse`) using the well known financial industry message protocol FIX. 
See for instance `http://www.onixs.biz/fix-dictionary/4.2/index.html` or `http://www.fixtradingcommunity.org/pg/resources/fiximate`.

FastParse is advertised to be nearly as fast as the fastest generic JVM parser out there (parboiled) and as convenient and flexible as the Scala parsers with
 natural clear error diagnostic reporting. It also integrates into the code directly unlike Lexx, YACC or ANTLR.

I am interested in application messages for the 4.2 variant of FIX protocol, which has been extensively used for trading North American equities for the past
 15 years. There are 5 main application messages identified by `msgType` as `D` (New Order Single), `F` (Cancel Request), `G` (Cancel Replace Request), `8` 
 (Execution Report), and `9` (Order Cancel Reject). For a decent number of equities applications, those 5 application messages in addition to Session layer 
 suffice. Less common but required FIX Messages `j` and `3` would also be interesting.

What I capture here is:
- distinguishing 5 application messages (`D`, `F`, `G`, `8`, and `9`)
- distinguishing header, body, and trailer (without accounting for payload or message length)
- identifying some of the tags on the 5 messages.
- Body tags are now external to code, thanks to Play's JSON.
- four distinct data types: `Int`, `Char`, `String`, and `Boolean`. The types are not handled in sufficiently generic way though.

Unit tests are meant to demonstrate some of the above features of the mini-parser.

Obvious limitations for those familiar with FIX (**this is a toy after all**):
- There is no serialization from the mapped data back to a FIX message.
- payload not handled to parse say about 100-200 characters to find message boundaries. Conceptually, this could be a first phase parse with the parser shown 
working at a second phase
- tag separator is in real life FIX a non-printable character 0x01, I use comma as a proxy with implied assumption that comma not be used in FIX tag content 
(in a real app, this would be different, however it makes for less mundane code and simpler presentation of test results with a nice printout)
- lots of FIX tags are not specified
- multiple versions of FIX protocol should be handled
- Administrative messages are not supported but are essential (Logon, Logout, retransmission requests, etc...). This could be viewed as a FIX engine feature 
rather than a parser

Obvious limitations for those familiar with Scala:
- native Scala types for `Char`, `Int` and `Boolean` should be supported in place of the Java ones I have. The Java ones should be seen as interim solution as I am 
having some difficulty with polymorphism and generic parameterized data types  here and there.
- Several hacks explicitly noted as such in code, it's work in progress.
 
