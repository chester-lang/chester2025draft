package chester.lsp

import munit.FunSuite
import org.eclipse.lsp4j.*
import java.util.concurrent.TimeUnit

class ChesterLanguageServerTest extends FunSuite {

  test("initialize should return correct capabilities") {
    val server = new ChesterLanguageServer()
    val params = new InitializeParams() // Dummy params

    // Simulate client connection if needed (though initialize doesn't seem to use it directly)
    // server.connect(mock[LanguageClient]) // Requires a mocking library like Mockito or ScalaMock

    val futureResult = server.initialize(params)

    // Wait for the future to complete (add a reasonable timeout)
    val result = futureResult.get(5, TimeUnit.SECONDS) // 5-second timeout

    assert(result != null, "InitializeResult should not be null")
    val capabilities = result.getCapabilities
    assert(capabilities != null, "ServerCapabilities should not be null")

    // Check a specific capability set in the initialize method
    val syncOptions = capabilities.getTextDocumentSync
    assert(syncOptions.isLeft, "TextDocumentSync should be specified by Kind (Left)")
    assertEquals(syncOptions.getLeft, TextDocumentSyncKind.Incremental, "TextDocumentSyncKind should be Incremental")

    val completionOptions = capabilities.getCompletionProvider
    assert(completionOptions != null, "CompletionProvider should be enabled (non-null options)")
    // We could add more specific checks on completionOptions.getTriggerCharacters etc. if needed

    val hoverOptions = capabilities.getHoverProvider
    assert(hoverOptions.isLeft && hoverOptions.getLeft == true, "HoverProvider should be enabled (Either.left(true))")

    // Add more assertions for other capabilities if needed
  }

} 