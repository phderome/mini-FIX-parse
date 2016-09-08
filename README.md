# mini-FIX-parse
This project explores the capabilities of FastParse (http://www.lihaoyi.com/fastparse/) using the well known financial industry message protocol FIX. See for instance http://www.onixs.biz/fix-dictionary/4.2/index.html or http://www.fixtradingcommunity.org/pg/resources/fiximate.

I am interested in application messages for the 4.2 variant of FIX protocol, which has been extensively used for trading North American equities for the past 15 years. There are 5 main application messages identified by msgType as D (New Order Single), F (Cancel Request), G (Cancel Replace Request), 8 (Execution Report), and 9 (Order Cancel Reject). For a decent number of equities applications, those 5 application messages in addition to Session layer suffice.

What I capture here is:
distinguishing 4 messages (D, F, G, and 8)
distinguishing header, body, and trailer (without accounting for payload or message length)
identifying some of the tags on the 4 messages.
three distinct data types: Int, String, and Boolean.

Unit tests are meant to demonstrate some of the above features of the mini-parser.
