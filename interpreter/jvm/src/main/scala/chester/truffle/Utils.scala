package chester.truffle

import chester.error.unreachable
import com.oracle.truffle.api.{CallTarget, TruffleLanguage}
import chester.reader._
import chester.syntax.core._
import chester.truffle.ChesterLang.ChesterRootNode
import chester.tyck._

object Utils {
  @throws[Exception]
  def parse(lang: ChesterLang, request: TruffleLanguage.ParsingRequest): CallTarget = {
    ChesterReader.parseTopLevel(FileNameAndContent(request.getSource.getPath, request.getSource.getCharacters.toString)).fold(err => ???, parsedBlock => Tycker.check(parsedBlock) match {
          case TyckResult.Success(result, _, _) => {
            val t: Term = result.wellTyped
            val root = new ChesterRootNode(lang, t)
            root.getCallTarget
          }
          case TyckResult.Failure(errors, _, _, _) => ???
          case _                                   => unreachable()
        })
  }
}
