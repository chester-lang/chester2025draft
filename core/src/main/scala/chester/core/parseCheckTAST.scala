package chester.core

import chester.error.*
import chester.parser.*
import chester.syntax.*
import chester.syntax.core.*
import chester.tyck.*
import chester.tyck.api.{NoopSemanticCollector, SemanticCollector}

def parseCheckTAST(
    source: ParserSource,
    ignoreLocation: Boolean = false,
    sementicCollector: SemanticCollector = NoopSemanticCollector,
    loadedModules: LoadedModules = LoadedModules.Empty
)(using reporter: Reporter[Problem]): chester.syntax.TAST = {
  // Parse the source code into an Expr using parseTopLevel
  Parser.parseTopLevel(source, ignoreLocation) match {
    case Right(expr) =>
      // Type-check the parsed expression
      checkTop(source.fileName, expr, reporter, loadedModules = loadedModules)
    case Left(error) =>
      // Report the parsing error
      reporter(error)

      // Return an empty TAST or handle accordingly
      TAST(
        fileName = source.fileName,
        module = DefaultModule,
        ast = BlockTerm(Vector.empty, UnitTerm()),
        ty = UnitType(),
        effects = NoEffect,
        problems = SeverityMap.Empty.copy(error = true)
      )
  }
}
