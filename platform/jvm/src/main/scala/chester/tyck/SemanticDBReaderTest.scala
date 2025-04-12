package chester.tyck

import chester.i18n.*

import scala.meta.internal.semanticdb.TextDocuments

object SemanticDBReaderTest {

  def main(args: Array[String]): Unit = {
    // Replace with the path to your .semanticdb file
    val semanticdbFilePath = "target/semanticdb/src/main/scala/example/Main.scala.semanticdb"

    // Read the SemanticDB file bytes
    val bytes = java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(semanticdbFilePath))

    // Parse the bytes into TextDocuments
    val textDocuments = TextDocuments.parseFrom(bytes)

    // Process each TextDocument
    textDocuments.documents.foreach { doc =>
      println(t"Processing file: ${doc.uri}")

      // Print symbols
      println("\nSymbols:")
      doc.symbols.foreach { symbolInfo =>
        println(t"Symbol: ${symbolInfo.symbol}")
        println(t"  Kind: ${symbolInfo.kind}")
        println(t"  Properties: ${symbolInfo.properties}")
        println(t"  Signature: ${symbolInfo.signature}")
      }

      // Print occurrences
      println("\nOccurrences:")
      doc.occurrences.foreach { occurrence =>
        val range = occurrence.range
          .map(r => t"${r.startLine}:${r.startCharacter} to ${r.endLine}:${r.endCharacter}")
          .getOrElse("No range")
        println(t"Symbol: ${occurrence.symbol}")
        println(t"  Role: ${occurrence.role}")
        println(t"  Range: $range")
      }
    }
  }
}
