package chester.truffle

import com.oracle.truffle.api.{CallTarget, TruffleLanguage}
import chester.parser.*

object Utils {
  @throws[Exception]
  def parse(request: TruffleLanguage.ParsingRequest): CallTarget = {
    val parsed = Parser.parseExpr(FileNameAndContent(request.getSource.getPath, request.getSource.getCharacters.toString))
    ???
  }
}
