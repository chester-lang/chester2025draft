package chester.targets.js

import chester.syntax.core.*
// Explicitly import necessary AST types from the same package
import chester.targets.js.{
  Expression,
  Meta,
  NumericLiteral,
  StringLiteral,
  BooleanLiteral,
  Identifier,
  FunctionExpression,
  BlockStatement,
  ReturnStatement,
  Parameter,
  IdentifierPattern
}
// Import Doc extension methods like render
import chester.utils.doc.*

/** Basic JavaScript/TypeScript backend. Transforms core.Term nodes into jsAST nodes.
  */
object Backend {

  // Use Default (capital D)
  given PrettierOptions = PrettierOptions.Default

  /** Transforms a Chester core.Term into a JavaScript AST Expression.
    */
  private def transform(term: Term): Expression = {
    // Use map instead of flatMap, should be equivalent for Option[TermMeta]
    val jsMeta = term.meta.map(_.sourcePos).map(sp => Meta(sp))

    term match {
      // Literals
      case IntegerTerm(value, meta) =>
        NumericLiteral(value.toDouble, jsMeta)

      case StringTerm(value, meta) =>
        StringLiteral(value, jsMeta)

      case BooleanTerm(value, meta) =>
        BooleanLiteral(value, jsMeta)

      // Function Definition
      case Function(ty, body, meta) =>
        // Extract parameter identifiers from the function type's telescopes
        // Wrap each Identifier in an IdentifierPattern, then in a Parameter
        val params = ty.telescopes.flatMap(_.args.map(arg => Parameter(IdentifierPattern(Identifier(arg.bind.name.toString)))))
        // Transform the body term
        val bodyExpr = transform(body)
        // JS FunctionExpression needs a BlockStatement, so wrap the body expression in a ReturnStatement
        // ReturnStatement expects Option[Expression]
        val bodyBlock = BlockStatement(List(ReturnStatement(Some(bodyExpr))))
        // Create the JS FunctionExpression
        FunctionExpression(
          id = None, // TODO: Handle named functions?
          params = params.toList,
          returnType = None, // TODO: Handle return type annotations?
          body = bodyBlock,
          async = false, // TODO: Handle async?
          generator = false, // TODO: Handle generators?
          meta = jsMeta
        )

      // Variable Reference
      case LocalV(name, ty, uniqId, meta) =>
        Identifier(name.toString, jsMeta)

      // TODO: Handle other term types (ToplevelV, Apply, If, etc.)
      case _ =>
        // Placeholder for unhandled cases
        // Return a simple identifier indicating the unhandled type for now
        Identifier(s"Unhandled_${term.getClass.getSimpleName}", jsMeta)
    }
  }

  /** Compiles a Chester core.Term to a JavaScript code string.
    */
  def compileToJs(term: Term): String = // Input is core.Term
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

  // Overload or alternative entry point if the absolute top-level input isn't a single Term
  // For example, if it's a sequence of top-level definitions or a specific 'Program' node
  // def compileProgramToJs(program: ???): String = { ... }
}
