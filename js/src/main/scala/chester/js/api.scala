package chester.js

import chester.elab.{DefaultElaborator, ElabOps}
import chester.error.{VectorReporter, *}
import chester.tyck.api.*

import chester.doc.consts.LightMode
import chester.reader.FileNameAndContent
import chester.utils.doc.ColorfulToHtml.colorfulToHtml
import chester.utils.doc.*
import chester.i18n.*
import chester.readerv1.ChesterReaderV1
import chester.error.TyckError

def runFileTopLevel(content: String, lightMode: Boolean): String = {
  given options: PrettierOptions =
    PrettierOptions.Default.updated(LightMode, lightMode)
  reporterToEither(
    ChesterReaderV1.parseTopLevel(
      FileNameAndContent("playground.chester", content)
    )
  ) match {
    case Right(parsedBlock) =>
      val reporter = new VectorReporter[TyckProblem]()
      given elabOps: ElabOps = ElabOps(reporter, NoopSemanticCollector)
      val tyckResult = DefaultElaborator.inferPure(parsedBlock)
      val problems = reporter.getReports
      if (problems.find(_.isInstanceOf[TyckError]).isEmpty) {
        // This is equivalent to TyckResult.Success case
        colorfulToHtml(ColorfulPrettyPrinter.render(tyckResult.wellTyped))
      } else {
        // This is equivalent to TyckResult.Failure case
        val errors = problems.collect { case e: TyckError => e }
        t"Failed to type check file: $errors"
      }
    case Left(error) =>
      error.message
  }
}
