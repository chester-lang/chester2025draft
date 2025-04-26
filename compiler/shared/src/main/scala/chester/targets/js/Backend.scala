package chester.targets.js

import chester.syntax.core.*
// Explicitly import necessary AST types from the same package
import chester.targets.js.{Expression, Meta, NumericLiteral, StringLiteral, BooleanLiteral, Identifier}
// Import Doc extension methods like render
import chester.utils.doc.*

/**
 * Basic JavaScript/TypeScript backend.
 * Transforms core.Term nodes into jsAST nodes.
 */
object Backend {

  // Use Default (capital D)
  given PrettierOptions = PrettierOptions.Default

  /**
   * Transforms a Chester core.Term into a JavaScript AST Expression.
   */
  private def transform(term: Term): Expression = {
    // Use map instead of flatMap, should be equivalent for Option[TermMeta]
    val jsMeta = term.meta.map(_.sourcePos).map(sp => Meta(sp))

    term match {
      // Use correct literal term names
      case IntegerTerm(value, meta) =>
        // Assuming value is BigInt, convert to Double for JS
        NumericLiteral(value.toDouble, jsMeta)

      case StringTerm(value, meta) =>
        StringLiteral(value, jsMeta)

      case BooleanTerm(value, meta) =>
        BooleanLiteral(value, jsMeta)

      // TODO: Need to find the correct representation for function definitions
      // case ??? => // Representation of Function Definition

      // TODO: Need to find the correct representation for variable references
      // case ??? => // Representation of Variable Reference

      // Placeholder for unhandled core.Term types
      case _ =>
        Identifier("undefined", jsMeta)
    }
  }

  /**
   * Compiles a Chester core.Term to a JavaScript code string.
   */
  def compileToJs(term: Term): String = { // Input is core.Term
    try {
      val jsAst = transform(term)
      // Use render extension method
      jsAst.toDoc.render(80)
    } catch {
      // MatchError might occur if pattern matching is incomplete
      case e: MatchError =>
        s"// Error: Backend transform failed - Unhandled core.Term node: ${e.getMessage}\n"
      case e: Exception =>
        s"// Error: Backend transform failed: ${e.getMessage}\n"
    }
  }

  // Overload or alternative entry point if the absolute top-level input isn't a single Term
  // For example, if it's a sequence of top-level definitions or a specific 'Program' node
  // def compileProgramToJs(program: ???): String = { ... }
}
