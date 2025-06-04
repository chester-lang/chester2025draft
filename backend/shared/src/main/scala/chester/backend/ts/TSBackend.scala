package chester.backend.ts

import chester.elab.*
import chester.utils.*
import chester.syntax.core.*
import chester.backend.{Backend, Typescript}
import chester.error.unreachable
import chester.uniqid.UniqidOf

import scala.collection.mutable
import scala.language.implicitConversions

implicit def metaConvert(x: Option[TermMeta]): Option[Meta] = x match {
  case Some(TermMeta(span)) => Some(Meta(Some(span)))
  case None                 => None
}

object IdentifierRules {
  // In JavaScript, identifiers can contain Unicode letters, $, _, and digits (0-9), but may not start with a digit.
  // An identifier differs from a string in that a string is data, while an identifier is part of the code.
  // In JavaScript, there is no way to convert identifiers to strings, but sometimes it is possible to parse strings into identifiers.
  def charToJS(codePoint: Int): String = {
    val str = new String(Character.toChars(codePoint))
    if (Character.isLetter(codePoint)) return str
    if (str == "$" || str == "_") return str
    if (Character.isDigit(codePoint)) return str
    if (str == "+") return "Plus"
    if (str == "-") return "Minus"
    if (str == "*") return "Star"
    if (str == "/") return "Slash"
    if (str == "=") return "Equals"
    if (str == "<") return "Less"
    if (str == ">") return "Greater"
    "$"
  }

  def convertToJSIdentifier(codePoints: Seq[Int]): String = {
    val sb = new StringBuilder()
    for (codePoint <- codePoints) {
      if (sb.isEmpty && Character.isDigit(codePoint)) {
        sb.append("_") // The First character cannot be a digit, so we prepend a _ sign
      }
      sb.append(charToJS(codePoint))
    }
    sb.toString()
  }

  def convertToJSIdentifier(name: String): String =
    convertToJSIdentifier(name.getCodePoints)

  def newSymbol(exisiting: Set[String], need: String): String = {
    if (!exisiting.contains(need)) return need
    var i = 1
    while (true) {
      val newName = s"${need}_$i"
      if (!exisiting.contains(newName)) return newName
      i += 1
    }
    unreachable()
  }

}

case class TSContext(map: mutable.HashMap[UniqidOf[LocalV], String], definedSymbols: Set[String] = Set.empty) {
  def convertAndAdd(name: String): (String, TSContext) = {
    val converted = IdentifierRules.convertToJSIdentifier(name)
    val newName = IdentifierRules.newSymbol(definedSymbols, converted)
    assume(!definedSymbols.contains(newName), s"Symbol $newName already exists in the context")
    (newName, copy(definedSymbols = definedSymbols + newName))
  }
}

object TSContext {
  def create(map: mutable.HashMap[UniqidOf[LocalV], String] = new mutable.HashMap()): TSContext =
    TSContext(map)
}

case object TSBackend extends Backend(Typescript) {
  def compileModule(mod: ZonkedTAST): Toplevel = {
    require(
      mod.ast.result match {
        case UnitTerm_(_) => true
        case _            => false
      },
      "Module must return unit"
    )
    implicit var ctx: TSContext = TSContext.create()
    Toplevel(mod.ast.statements.map { stmt =>
      val (compiledStmt, newCtx) = compileStmt(stmt)(using ctx)
      ctx = newCtx
      compiledStmt
    })
  }
  def compileType(term: Term)(using ctx: TSContext): TSType = term match {
    case IntType(meta)  => NumberType(meta)
    case UIntType(meta) => NumberType(meta)
    case _              => ???
  }
  def compileExpr(term: Term)(using ctx: TSContext): TSExpr = term match {
    case IntTerm(value, meta)  => DoubleExpr(value.toDouble, meta)
    case UIntTerm(value, meta) => DoubleExpr(value.toDouble, meta)
    case localV: LocalV =>
      val name = ctx.map.getOrElse(
        localV.uniqId,
        throw new IllegalArgumentException(s"Variable ${localV.name} not found in context")
      )
      IdentifierExpr(name, localV.meta)
    case _ => ???
  }
  def introduceLetVar(let: LetStmtTerm)(using ctx: TSContext): (String, TSContext) = {
    // TODO: handle shadowing and uniqueness and javascript reserved words and javascript reserved symbols and a lot more
    val (name, ctx1) = ctx.convertAndAdd(let.localv.name)
    assume(!ctx.map.contains(let.localv.uniqId), s"Variable $name already exists in the context")
    val _ = ctx.map.put(let.localv.uniqId, name)
    (name, ctx1)
  }
  def useVar(localV: LocalV)(using ctx: TSContext): String =
    ctx.map.getOrElse(localV.uniqId, throw new IllegalArgumentException(s"Variable ${localV.name} not found in context"))
  def compileStmt(stmt: StmtTerm)(using ctx: TSContext): (TSStmt, TSContext) = stmt match {
    case let: LetStmtTerm =>
      val (name, ctx) = introduceLetVar(let)
      (
        ConstStmt(
          name = name,
          ty = Some(compileType(let.ty)),
          value = compileExpr(let.value),
          meta = let.meta
        ),
        ctx
      )
    case _ => ???
  }
}
