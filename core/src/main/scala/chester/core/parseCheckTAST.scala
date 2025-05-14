package chester.core

import chester.error.*
import chester.reader.*
import chester.readerv2.ChesterReaderV2
import chester.syntax.*
import chester.syntax.core.*
import chester.tyck.*
import chester.tyck.api.{NoopSemanticCollector, SemanticCollector}

def parseCheckTAST(
    source: ParserSource,
    ignoreLocation: Boolean = false,
    sementicCollector: SemanticCollector = NoopSemanticCollector,
    loadedModules: LoadedModules = LoadedModules.Empty
)(using reporter: Reporter[Problem]): chester.syntax.TAST =
  // Parse the source code into an Expr using parseTopLevel
  ChesterReaderV2
    .parseTopLevel(source, ignoreLocation)
    .fold(
      { error =>
        reporter.report(error)

        // Return an empty TAST or handle accordingly
        TAST(
          fileName = source.fileName,
          module = DefaultModule,
          ast = BlockTerm(Vector.empty, UnitTerm_(meta = None), meta = None),
          ty = UnitType(meta = None),
          effects = Effects.Empty,
          problems = SeverityMap.Empty.copy(error = true)
        )
      },
      expr => checkTop(source.fileName, expr, reporter, loadedModules = loadedModules)
    )
