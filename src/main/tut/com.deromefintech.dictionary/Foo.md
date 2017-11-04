
Here's how to parse some FIX:
```tut
import com.deromefintech.dictionary.FIXDictionary._
import fastparse.core.Parsed
object TestFoo {
  def correctTagSeparator(data: String) = data.replace(',', SOHAsString(0))
}

FIXMsgDP.parse(TestFoo.correctTagSeparator("8=FIX.4.2,35=D,43=Y,54=0,40=0,55=BMO,10=100"))
```