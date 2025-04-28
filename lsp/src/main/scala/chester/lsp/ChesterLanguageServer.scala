package chester.lsp

import chester.error.*
import chester.reader.*
import chester.syntax.core.*
import chester.tyck.api.*
import chester.tyck.*
import chester.utils.{StringIndex, WithUTF16}
import _root_.io.github.iltotore.iron.*
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.*
import org.log4s.*
import chester.i18n.*
import chester.readerv1.ChesterReader
import chester.readerv2.ChesterReaderV2

import java.util.List as JList
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*

class ChesterLanguageServer extends LanguageServer with TextDocumentService with WorkspaceService {
  enableDebug()
  private val logger = getLogger

  private var client: LanguageClient = uninitialized

  // Store documents' content
  private val documents = mutable.Map[String, DocumentInfo]()

  def connect(client: LanguageClient): Unit = {
    this.client = client
    logger.info("Language client connected to the server")
  }

  override def initialize(
      params: InitializeParams
  ): CompletableFuture[InitializeResult] = {
    logger.info(t"Initializing with params: $params")
    val capabilities = new ServerCapabilities()
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Incremental)
    capabilities.setCompletionProvider(
      new CompletionOptions(false, List(".", ":").asJava)
    )
    capabilities.setHoverProvider(true)
    capabilities.setDefinitionProvider(true)
    capabilities.setReferencesProvider(true)
    capabilities.setDocumentSymbolProvider(true)
    capabilities.setWorkspaceSymbolProvider(true)
    capabilities.setCodeActionProvider(true)
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  override def getTextDocumentService: TextDocumentService = this

  override def getWorkspaceService: WorkspaceService = this

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    logger.info(t"Document opened: $uri")
    val text = params.getTextDocument.getText

    // Process the document and get TyckResult and diagnostics
    val (tyckResult, collected, diagnostics) = processDocument(uri, text)

    // Store the DocumentInfo in the documents map
    documents.synchronized {
      documents(uri) = DocumentInfo(
        content = text,
        symbols = collected,
        tyckResult = tyckResult
      )
    }

    // Publish diagnostics
    client.publishDiagnostics(
      new PublishDiagnosticsParams(uri, diagnostics.asJava)
    )
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    logger.info(t"Document changed: $uri")
    val changes = params.getContentChanges.asScala.toSeq

    // Update the document content
    val updatedText = applyChanges(uri, changes)

    // Process the updated document
    val (tyckResult, collected, diagnostics) = processDocument(uri, updatedText)

    // Update the DocumentInfo with the new content and TyckResult
    documents.synchronized {
      documents.get(uri).foreach { _ =>
        documents(uri) = DocumentInfo(
          content = updatedText,
          symbols = collected,
          tyckResult = tyckResult
        )
      }
    }

    // Publish diagnostics
    client.publishDiagnostics(
      new PublishDiagnosticsParams(uri, diagnostics.asJava)
    )
  }

  private def applyChanges(
      uri: String,
      changes: Seq[TextDocumentContentChangeEvent]
  ): String = {
    val currentText = documents.synchronized {
      documents.get(uri).map(_.content).getOrElse("")
    }

    val updatedText = changes.foldLeft(currentText) { (text, change) =>
      if (change.getRange == null) {
        // The change is the new full content
        change.getText
      } else {
        // Apply incremental change
        val startOffset = getOffset(text, change.getRange.getStart)
        val endOffset = getOffset(text, change.getRange.getEnd)
        text.substring(0, startOffset) + change.getText + text.substring(
          endOffset
        )
      }
    }

    updatedText // Return the updated text without modifying documents
  }

  private def getOffset(text: String, position: Position): Int = {
    var offset = 0
    var line = 0
    var charIndex = 0

    while (offset < text.length && line < position.getLine) {
      if (text.charAt(offset) == '\n') {
        line += 1
      }
      offset += 1
    }

    while (offset < text.length && charIndex < position.getCharacter) {
      if (text.charAt(offset) == '\n') {
        // Shouldn't happen, but safeguard
        return offset
      }
      offset += 1
      charIndex += 1
    }

    offset
  }

  private def getLineStartOffset(text: String, lineNumber: Int): Int = {
    val lines = text.split('\n')
    val safeLineNumber = lineNumber.min(lines.length - 1).max(0)
    lines.take(safeLineNumber).map(_.length + 1).sum // +1 for '\n' character
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri

    // Remove the document from the store
    val _ = documents.remove(uri)

    // Clear diagnostics for the closed document
    client.publishDiagnostics(
      new PublishDiagnosticsParams(uri, List.empty[Diagnostic].asJava)
    )
  }

  private def processDocument(
      uri: String,
      text: String
  ): (TyckResult[Unit, Judge], Vector[CollectedSymbol], List[Diagnostic]) = {
    val parseResult = ChesterReaderV2.parseTopLevel(FileNameAndContent(uri, text))
    val collecter = new VectorSemanticCollector()

    parseResult.fold(
      { parseError =>
        val range = new Range(
          new Position(parseError.pos.line, parseError.pos.column.utf16),
          new Position(parseError.pos.line, parseError.pos.column.utf16)
        )
        val diagnostic = new Diagnostic(
          range,
          parseError.message,
          DiagnosticSeverity.Error,
          "ChesterLanguageServer"
        )
        val tyckResult = TyckResult(
          state = (),
          result = Judge(ErrorTerm(parseError, meta = None), ErrorTerm(parseError, meta = None), Effects.Empty)
        )
        (tyckResult, Vector(), List(diagnostic))
      },
      { parsedExpr =>
        val tyckResult = Tycker.check(parsedExpr, sementicCollector = collecter)

        // Generate diagnostics from the TyckResult
        val diagnostics = tyckResult match {
          case TyckResult.Success(_, _, warnings) =>
            // Process warnings
            warnings.map { warning =>
              val range = warning.sourcePos
                .map(rangeFromSourcePos)
                .getOrElse(new Range(new Position(0, 0), new Position(0, 0)))

              new Diagnostic(
                range,
                warning.getMessage,
                DiagnosticSeverity.Warning,
                "ChesterLanguageServer"
              )
            }.toList

          case TyckResult.Failure(errors, warnings, _, _) =>
            // Combine errors and warnings into diagnostics
            val errorDiagnostics = errors.map { error =>
              val range = error.sourcePos
                .map(rangeFromSourcePos)
                .getOrElse(new Range(new Position(0, 0), new Position(0, 0)))

              new Diagnostic(
                range,
                error.getMessage,
                DiagnosticSeverity.Error,
                "ChesterLanguageServer"
              )
            }

            val warningDiagnostics = warnings.map { warning =>
              val range = warning.sourcePos
                .map(rangeFromSourcePos)
                .getOrElse(new Range(new Position(0, 0), new Position(0, 0)))

              new Diagnostic(
                range,
                warning.getMessage,
                DiagnosticSeverity.Warning,
                "ChesterLanguageServer"
              )
            }

            (errorDiagnostics ++ warningDiagnostics).toList
        }

        (tyckResult, collecter.get, diagnostics)
      }
    )
  }

  override def completion(
      params: CompletionParams
  ): CompletableFuture[Either[JList[CompletionItem], CompletionList]] = {
    val position = params.getPosition
    val textDocument = params.getTextDocument.getUri
    val completions = provideCompletions(textDocument, position)
    val completionList = new CompletionList(completions.asJava)
    CompletableFuture.completedFuture(Either.forRight(completionList))
  }

  private def provideCompletions(
      _uri: String,
      _position: Position
  ): List[CompletionItem] =
    // Implement logic to provide code completions based on the position
    List(new CompletionItem("exampleCompletion"))

  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    val position = params.getPosition
    val uri = params.getTextDocument.getUri
    val contents = provideHoverInformation(uri, position)
    CompletableFuture.completedFuture(new Hover(contents))
  }

  private def provideHoverInformation(
      _uri: String,
      _position: Position
  ): MarkupContent = {
    // Logic to fetch hover information based on position
    val content = new MarkupContent()
    content.setKind("markdown")
    content.setValue("Detailed information about the symbol at the cursor.")
    content
  }

  // LanguageServer methods
  override def exit(): Unit = {
    // Handle server exit logic here
  }

  override def shutdown(): CompletableFuture[Object] =
    // Handle server shutdown logic here
    CompletableFuture.completedFuture(null)

  // TextDocumentService methods
  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    // Handle text document save event here
  }

  // WorkspaceService methods
  override def didChangeConfiguration(
      params: DidChangeConfigurationParams
  ): Unit = {
    // Handle workspace configuration change here
  }

  override def didChangeWatchedFiles(
      params: DidChangeWatchedFilesParams
  ): Unit = {
    // Handle watched files change event here
  }

  private def sourcePosFromLSP(
      uri: String,
      position: Position
  ): Option[SourcePos] = {
    logger.debug(
      t"Converting LSP position to source position for URI: $uri at position: $position"
    )
    documents.synchronized {
      documents.get(uri)
    } match {
      case Some(document) =>
        val text = document.content
        val stringIndex = StringIndex(text)

        // LSP positions are in UTF-16 code units
        val line = position.getLine
        val utf16Column = position.getCharacter

        logger.debug(
          t"Calculating offsets for line: $line, UTF-16 column: $utf16Column"
        )

        val lineStartOffset = getLineStartOffset(text, line)
        val charIndexUtf16 = lineStartOffset + utf16Column

        logger.debug(
          t"Line start offset: $lineStartOffset, char index UTF-16: $charIndexUtf16"
        )

        val codepointIndex =
          stringIndex.charIndexToUnicodeIndex(charIndexUtf16.refineUnsafe)

        // Get the line and column with both Unicode code points and UTF-16 code units
        val lineAndColumn =
          stringIndex.charIndexToLineAndColumnWithUTF16(charIndexUtf16)

        val pos = Pos(
          index = WithUTF16(codepointIndex, charIndexUtf16.refineUnsafe),
          line = lineAndColumn.line,
          column = lineAndColumn.column
        )
        val range = RangeInFile(start = pos, end = pos)
        val source = Source(FileNameAndContent(uri, text))
        val sourcePos = SourcePos(source, range)

        logger.debug(t"Generated SourcePos: $sourcePos")

        Some(sourcePos)
      case None =>
        logger.warn(t"No document content found for URI: $uri")
        None
    }
  }

  private def rangeFromSourcePos(sourcePos: SourcePos): Range = {
    val startLine = sourcePos.range.start.line
    val startCharacter = sourcePos.range.start.column.utf16
    val endLine = sourcePos.range.end.line
    val endCharacter = sourcePos.range.end.column.utf16

    val start = new Position(startLine, startCharacter)
    val end = new Position(endLine, endCharacter)
    new Range(start, end)
  }

  override def definition(
      params: DefinitionParams
  ): CompletableFuture[
    Either[JList[? <: Location], JList[? <: LocationLink]]
  ] =
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument.getUri
      val position = params.getPosition

      logger.debug(t"Definition requested for URI: $uri at position: $position")

      val documentOpt = documents.synchronized {
        documents.get(uri)
      }

      documentOpt match {
        case Some(document) =>
          logger.debug(t"Document found for URI: $uri")
          sourcePosFromLSP(uri, position) match {
            case Some(sourcePos) =>
              logger.debug(t"Source position obtained: $sourcePos")
              val symbolOpt = document.symbols.find { sym =>
                positionWithin(sym.definedOn.sourcePos.get, sourcePos) ||
                sym.referencedOn.exists(ref => positionWithin(ref.sourcePos.get, sourcePos))
              }
              symbolOpt match {
                case Some(symbolInfo) =>
                  logger.debug(t"Symbol found: ${symbolInfo.name}")
                  val location = new Location(
                    symbolInfo.definedOn.sourcePos.get.fileName,
                    rangeFromSourcePos(symbolInfo.definedOn.sourcePos.get)
                  )
                  val locations = java.util.Collections.singletonList(location)
                  Either.forLeft(locations)
                case None =>
                  logger.warn(
                    t"No symbol found at position: $sourcePos in document: $uri"
                  )
                  if (logger.isTraceEnabled) {
                    logger.trace("Available symbols:")
                    document.symbols.foreach { sym =>
                      logger.trace(t"Symbol name: ${sym.name}")
                      sym.definedOn.sourcePos.foreach(pos => logger.trace(t"  Defined at: $pos"))
                      sym.referencedOn.foreach(ref => ref.sourcePos.foreach(pos => logger.trace(t"  Referenced at: $pos")))
                    }
                  }
                  Either.forLeft(java.util.Collections.emptyList())
              }
            case None =>
              logger.warn(
                t"Could not obtain source position for URI: $uri at position: $position"
              )
              Either.forLeft(java.util.Collections.emptyList())
          }
        case None =>
          logger.warn(t"No document found for URI: $uri")
          Either.forLeft(java.util.Collections.emptyList())
      }
    }

  private def positionWithin(pos1: SourcePos, pos2: SourcePos): Boolean =
    pos1.source.fileName == pos2.source.fileName &&
      comparePositions(pos1.range.start, pos2.range.start) <= 0 &&
      comparePositions(pos1.range.end, pos2.range.end) >= 0

  private def comparePositions(p1: Pos, p2: Pos): Int =
    if (p1.line != p2.line) {
      p1.line.compareTo(p2.line)
    } else {
      p1.column.utf16.compareTo(p2.column.utf16)
    }

  override def references(
      params: ReferenceParams
  ): CompletableFuture[JList[? <: Location]] =
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument.getUri
      val position = params.getPosition

      val documentOpt = documents.synchronized {
        documents.get(uri)
      }

      documentOpt match {
        case Some(document) =>
          sourcePosFromLSP(uri, position) match {
            case Some(sourcePos) =>
              val symbolOpt = document.symbols.find { sym =>
                positionWithin(
                  sym.definedOn.sourcePos.get,
                  sourcePos
                ) || sym.referencedOn.exists(ref => positionWithin(ref.sourcePos.get, sourcePos))
              }
              symbolOpt match {
                case Some(symbolInfo) =>
                  val locations = (symbolInfo.referencedOn.flatMap(
                    _.sourcePos
                  ) ++ symbolInfo.definedOn.sourcePos).map(refPos => new Location(refPos.fileName, rangeFromSourcePos(refPos)))
                  new java.util.ArrayList(locations.toList.asJava)
                case None =>
                  java.util.Collections.emptyList()
              }
            case None =>
              java.util.Collections.emptyList()
          }

        case None =>
          java.util.Collections.emptyList()
      }
    }

  override def documentSymbol(
      params: DocumentSymbolParams
  ): CompletableFuture[JList[Either[SymbolInformation, DocumentSymbol]]] =
    CompletableFuture.supplyAsync(() =>
      // Implement document symbol lookup logic here
      new java.util.ArrayList[Either[SymbolInformation, DocumentSymbol]]()
    )

  override def codeAction(
      params: CodeActionParams
  ): CompletableFuture[JList[Either[Command, CodeAction]]] =
    CompletableFuture.supplyAsync(() =>
      // Implement code action logic here
      new java.util.ArrayList[Either[Command, CodeAction]]()
    )

  override def symbol(
      params: WorkspaceSymbolParams
  ): CompletableFuture[Either[JList[? <: SymbolInformation], JList[? <: WorkspaceSymbol]]] =
    CompletableFuture.supplyAsync { () =>
      val query = params.getQuery.toLowerCase
      val allSymbols = documents.synchronized {
        documents.values.flatMap(_.symbols).toList
      }

      val matchingSymbols = allSymbols
        .withFilter { symbol =>
          symbol.name.toLowerCase.contains(query) &&
          symbol.definedOn.sourcePos.nonEmpty
        }
        .map(tyckSymbolToLSP)

      // Return the list of WorkspaceSymbol wrapped in an Either
      Either.forRight[JList[? <: SymbolInformation], JList[? <: WorkspaceSymbol]](matchingSymbols.asJava)
    }

  private def tyckSymbolToLSP(symbol: CollectedSymbol): WorkspaceSymbol = {
    val sp = symbol.definedOn.sourcePos.get
    val location = new Location(
      sp.fileName,
      rangeFromSourcePos(sp)
    )

    new WorkspaceSymbol(
      symbol.name,
      SymbolKind.Variable,
      Either.forLeft(location)
    )
  }

  override def setTrace(value: SetTraceParams): Unit =
    logger.debug(t"(ignored) Required Trace level: ${value.getValue}")
}

case class DocumentInfo(
    content: String,
    symbols: Vector[CollectedSymbol],
    tyckResult: TyckResult[Unit, Judge]
) {}
