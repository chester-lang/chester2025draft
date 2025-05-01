package chester.truffle

import com.oracle.truffle.api.{CallTarget, TruffleLanguage}
import chester.reader.*
import chester.readerv2.ChesterReaderV2
import chester.syntax.core.*
import chester.truffle.ChesterLang.ChesterRootNode
import chester.tyck.*

object Utils {
  @throws[Exception]
  def parse(lang: ChesterLang, request: TruffleLanguage.ParsingRequest): CallTarget =
    ChesterReaderV2
      .parseTopLevel(FileNameAndContent(request.getSource.getPath, request.getSource.getCharacters.toString))
      .fold(
        _err => ???,
        parsedBlock =>
          val tyckResult = Tycker.check(parsedBlock)
          if (tyckResult.errorsEmpty) {
            // This is equivalent to TyckResult.Success case
            val result = tyckResult.result
            val t: Term = result.wellTyped
            val root = new ChesterRootNode(lang, t)
            root.getCallTarget
          } else {
            // This is equivalent to TyckResult.Failure case
            ???
          }
      )
}
