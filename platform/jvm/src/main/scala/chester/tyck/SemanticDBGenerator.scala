package chester.tyck

import chester.core.parseCheckTAST
import chester.error.*
import chester.reader
import chester.tyck.api.{CollectedSymbol, VectorSemanticCollector}
import chester.syntax.*
import chester.syntax.concrete.Expr
import os.*
import chester.reader.given

import scala.meta.internal.semanticdb.*

class SemanticDBGenerator extends VectorSemanticCollector {

  // Process a file or directory
  def processPath(path: Path): Unit =
    if (os.isDir(path)) {
      val files = os
        .walk(path)
        .filter(p => os.isFile(p) && p.ext == "chester")
      files.foreach(processFile)
    } else {
      processFile(path)
    }

  // Process a single file
  def processFile(file: Path): Unit = {
    val parserSource = reader.FilePath(file.toString)
    given reporter: Reporter[Problem] = StdErrReporter
    // Parse and type-check the source code using parseCheckTAST
    val _ = parseCheckTAST(parserSource, sementicCollector = this)
  }

  // Convert CollectedSymbols to SemanticDB TextDocument
  def toSemanticDB(docUri: String): TextDocument = {
    val symbols = get.map(symbolInformation)
    val occurrences = get.flatMap { sym =>
      val defOccurrence = symbolOccurrence(sym.definedOn, sym.id.toString, SymbolOccurrence.Role.DEFINITION)
      val refOccurrences = sym.referencedOn.map(expr => symbolOccurrence(expr, sym.id.toString, SymbolOccurrence.Role.REFERENCE))
      defOccurrence +: refOccurrences
    }

    TextDocument(
      schema = Schema.SEMANTICDB4,
      uri = docUri,
      language = Language.SCALA,
      symbols = symbols,
      occurrences = occurrences
    )
  }

  // Convert CollectedSymbol to SymbolInformation
  private def symbolInformation(sym: CollectedSymbol): SymbolInformation =
    SymbolInformation(
      symbol = sym.id.toString,
      language = Language.SCALA,
      kind = SymbolInformation.Kind.UNKNOWN_KIND,
      displayName = sym.call.name,
      signature = Signature.Empty
    )

  // Create SymbolOccurrence from Expr
  private def symbolOccurrence(
      expr: Expr,
      symbol: String,
      role: SymbolOccurrence.Role
  ): SymbolOccurrence =
    SymbolOccurrence(
      range = exprRange(expr),
      symbol = symbol,
      role = role
    )

  // Convert Expr to Range
  // TODO: check if should use utf16 or codepoints
  private def exprRange(expr: Expr): Option[Range] =
    expr.sourcePos.map { pos =>
      Range(
        startLine = pos.range.start.line,
        startCharacter = pos.range.start.column.utf16,
        endLine = pos.range.end.line,
        endCharacter = pos.range.end.column.utf16
      )
    }

  // Save the SemanticDB TextDocument to a file
  def saveSemanticDB(docUri: String, outputPath: String): Unit = {
    val textDocument = toSemanticDB(docUri)
    val textDocuments = TextDocuments(Seq(textDocument))

    val path = os.Path(outputPath)
    val bytes = textDocuments.toByteArray
    os.makeDir.all(path / os.up)
    os.write.over(path, bytes)
  }
}
