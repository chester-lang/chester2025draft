package chester.js

import chester.doc.consts.LightMode
import chester.reader.FileNameAndContent
import chester.tyck.*
import chester.utils.doc.ColorfulToHtml.colorfulToHtml
import chester.utils.doc.*
import chester.i18n.*
import chester.readerv2.ChesterReaderV2

def runFileTopLevel(content: String, lightMode: Boolean): String = {
  given options: PrettierOptions =
    PrettierOptions.Default.updated(LightMode, lightMode)
  ChesterReaderV2.parseTopLevel(
    FileNameAndContent("playground.chester", content)
  ) match {
    case Right(parsedBlock) =>
      val tyckResult = Tycker.check(parsedBlock)
      if (tyckResult.errorsEmpty) {
        // This is equivalent to TyckResult.Success case
        val result = tyckResult.result
        colorfulToHtml(ColorfulPrettyPrinter.render(result.wellTyped))
      } else {
        // This is equivalent to TyckResult.Failure case
        val errors = tyckResult.problems.collect { case e: TyckError => e }
        t"Failed to type check file: $errors"
      }
    case Left(error) =>
      error.message
  }
}
