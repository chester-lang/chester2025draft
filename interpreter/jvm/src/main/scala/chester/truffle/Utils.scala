package chester.truffle

import com.oracle.truffle.api.{CallTarget, TruffleLanguage}
import chester.parser.*
import chester.tyck.*

object Utils {
  @throws[Exception]
  def parse(request: TruffleLanguage.ParsingRequest): CallTarget = {
    Parser.parseTopLevel(FileNameAndContent(request.getSource.getPath, request.getSource.getCharacters.toString)) match {
      case Left(err) => ???
      case Right(parsedBlock) =>
        Tycker.check(parsedBlock) match {
          case TyckResult.Success(result, _, _)    => ???
          case TyckResult.Failure(errors, _, _, _) => ???
        }
    }
  }
}
