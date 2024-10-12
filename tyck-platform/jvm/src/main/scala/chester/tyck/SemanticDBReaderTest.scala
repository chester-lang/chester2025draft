package chester.tyck

import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.Scala._

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
      println(s"Processing file: ${doc.uri}")

      // Print symbols
      println("\nSymbols:")
      doc.symbols.foreach { symbolInfo =>
        println(s"Symbol: ${symbolInfo.symbol}")
        println(s"  Kind: ${symbolInfo.kind}")
        println(s"  Properties: ${symbolInfo.properties}")
        println(s"  Signature: ${symbolInfo.signature}")
      }

      // Print occurrences
      println("\nOccurrences:")
      doc.occurrences.foreach { occurrence =>
        val range = occurrence.range
          .map { r =>
            s"${r.startLine}:${r.startCharacter} to ${r.endLine}:${r.endCharacter}"
          }
          .getOrElse("No range")
        println(s"Symbol: ${occurrence.symbol}")
        println(s"  Role: ${occurrence.role}")
        println(s"  Range: $range")
      }
    }
  }
}
