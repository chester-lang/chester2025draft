package chester.js

import chester.doc.consts.LightMode
import chester.reader.{ChesterReader, FileNameAndContent}
import chester.tyck.*
import chester.utils.doc.ColorfulToHtml.colorfulToHtml
import chester.utils.doc.*
import chester.i18n.*

def runFileTopLevel(content: String, lightMode: Boolean): String = {
  given options: PrettierOptions =
    PrettierOptions.Default.updated(LightMode, lightMode)
  ChesterReader.parseTopLevel(
    FileNameAndContent("playground.chester", content)
  ) match {
    case Right(parsedBlock) =>
      Tycker.check(parsedBlock) match {
        case TyckResult.Success(result, _, _) =>
          colorfulToHtml(ColorfulPrettyPrinter.render(result.wellTyped))
        case TyckResult.Failure(errors, _, _, _) =>
          t"Failed to type check file: $errors"
      }
    case Left(error) =>
      error.message
  }
}
