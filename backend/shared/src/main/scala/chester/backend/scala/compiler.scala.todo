package chester.translator

import chester.syntax.core._
import chester.uniqid._
import scala.meta._
import scala.collection.mutable

class TermTranslator {

  // Map from UniqId to unique names to prevent naming clashes
  private val idNameMap = mutable.Map[UniqId, String]()
  private val nameCounts = mutable.Map[String, Int]()

  // Main translation method
  def translate(term: Term): scala.meta.Stat = term match {

    // Let Statement Term
    case LetStmtTerm(localv, value, ty, _) =>
      val varName = getUniqueName(localv.name.toString, localv.uniqId)
      val valueTree = translate(value)
      val tyTree = translateType(ty)
      q"val ${Pat.Var(Term.Name(varName))}: $tyTree = $valueTree"

    // Def Statement Term
    case DefStmtTerm(localv, value, ty, _) =>
      val varName = getUniqueName(localv.name.toString, localv.uniqId)
      val valueTree = translate(value)
      val tyTree = translateType(ty)
      q"def ${Term.Name(varName)}: $tyTree = $valueTree"

    // Expression Statement Term
    case ExprStmtTerm(expr, ty, _) =>
      translate(expr)

    // Non-local or Local Return
    case NonlocalOrLocalReturn(value, _) =>
      val valueTree = translate(value)
      q"return $valueTree"

    // Tuple Type
    case TupleType(types, _) =>
      val typeTrees = types.map(translateType)
      t"(..$typeTrees)"

    // Tuple Term
    case TupleTerm(values, _) =>
      val valueTrees = values.map(translate)
      q"(..$valueTrees)"

    // Block Term
    case BlockTerm(statements, result, _) =>
      val stmtTrees = statements.map {
        case stmt: StmtTerm => translate(stmt)
      }
      val resultTree = translate(result)
      q"{ ..$stmtTrees; $resultTree }"

    // Annotation
    case Annotation(term, tyOpt, effectsOpt, _) =>
      val termTree = translate(term)
      val tyTreeOpt = tyOpt.map(translateType)
      val effectsTreeOpt = effectsOpt.map(translateEffects)
      (tyTreeOpt, effectsTreeOpt) match {
        case (Some(tyTree), _) =>
          q"$termTree: $tyTree"
        case (None, Some(effectsTree)) =>
          q"$termTree /* effects: $effectsTree */"
        case _ => termTree
      }

    // Identifier
    case Identifier(name, tyOpt, uniqId, _) =>
      val uniqueName = getUniqueName(name.name, uniqId)
      Term.Name(uniqueName)

    // Function Term
    case Function(args, body, _) =>
      val paramNames = args.map { arg =>
        val paramName = getUniqueName(arg.name.name, arg.name.uniqId)
        val paramType = translateType(arg.ty)
        param"${Term.Name(paramName)}: $paramType"
      }
      val bodyTree = translate(body)
      q"(..$paramNames) => $bodyTree"

    // Apply Term
    case Apply(func, args, _) =>
      val funcTree = translate(func)
      val argTrees = args.map(translate)
      q"$funcTree(..$argTrees)"

    // Literal
    case Literal(value, _) =>
      value match {
        case i: Int     => Lit.Int(i)
        case s: String  => Lit.String(s)
        case b: Boolean => Lit.Boolean(b)
        case _          => Lit.Null()
      }

    // Other Terms (Extend with other cases as per your Term definitions)
    case _ =>
      sys.error(s"Translation not implemented for term: $term")
  }

  // Helper method to translate types
  private def translateType(ty: Term): scala.meta.Type = ty match {
    case Identifier(name, _, _, _) => Type.Name(name.name)
    case TupleType(types, _)       => t"(..${types.map(translateType)})"
    case _                         => Type.Name("Any")
  }

  // Helper method to translate effects (placeholder implementation)
  private def translateEffects(effects: EffectsM): scala.meta.Tree = {
    // Implement translations for effects if needed
    q"/* effects */"
  }

  // Helper method to get a unique name
  private def getUniqueName(baseName: String, uniqId: UniqId): String = {
    idNameMap.getOrElseUpdate(uniqId, {
      val count = nameCounts.getOrElse(baseName, 0)
      nameCounts.update(baseName, count + 1)
      if (count == 0) baseName else s"${baseName}_$count"
    })
  }

  // Overloaded translate method for expressions
  private def translate(term: Term): scala.meta.Term = term match {

    // Handle Identifier as Term.Name
    case Identifier(name, tyOpt, uniqId, _) =>
      val uniqueName = getUniqueName(name.name, uniqId)
      Term.Name(uniqueName)

    // Function Term
    case Function(args, body, _) =>
      val paramNames = args.map { arg =>
        val paramName = getUniqueName(arg.name.name, arg.name.uniqId)
        val paramType = translateType(arg.ty)
        param"${Term.Name(paramName)}: $paramType"
      }
      val bodyTree = translate(body)
      q"(..$paramNames) => $bodyTree"

    // Apply Term
    case Apply(func, args, _) =>
      val funcTree = translate(func)
      val argTrees = args.map(translate)
      q"$funcTree(..$argTrees)"

    // Tuple Term
    case TupleTerm(values, _) =>
      val valueTrees = values.map(translate)
      q"(..$valueTrees)"

    // Literal
    case Literal(value, _) =>
      value match {
        case i: Int     => Lit.Int(i)
        case s: String  => Lit.String(s)
        case b: Boolean => Lit.Boolean(b)
        case _          => Lit.Null()
      }

    // Annotation
    case Annotation(term, tyOpt, effectsOpt, _) =>
      val termTree = translate(term)
      val tyTreeOpt = tyOpt.map(translateType)
      val effectsTreeOpt = effectsOpt.map(translateEffects)
      (tyTreeOpt, effectsTreeOpt) match {
        case (Some(tyTree), _) =>
          q"$termTree: $tyTree"
        case (None, Some(effectsTree)) =>
          q"$termTree /* effects: $effectsTree */"
        case _ => termTree
      }

    // Block Term
    case BlockTerm(statements, result, _) =>
      val stmtTrees = statements.map {
        case stmt: StmtTerm => translate(stmt)
      }
      val resultTree = translate(result)
      q"{ ..$stmtTrees; $resultTree }"

    // Other Terms
    case _ =>
      sys.error(s"Translation not implemented for term: $term")
  }
}