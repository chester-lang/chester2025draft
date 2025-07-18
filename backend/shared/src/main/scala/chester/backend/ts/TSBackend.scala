package chester.backend.ts

import chester.elab.*
import chester.utils.*
import chester.syntax.core.*
import chester.backend.{Backend, Typescript}
import chester.error.unreachable
import chester.uniqid.UniqidOf
import chester.i18n.*

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
    val str = codepointToString(codePoint)
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

case class TSContext(map: mutable.HashMap[UniqidOf[LocalVar], String], definedSymbols: Set[String] = Set.empty) {
  def convertAndAdd(name: String): (String, TSContext) = {
    val converted = IdentifierRules.convertToJSIdentifier(name)
    val newName = IdentifierRules.newSymbol(definedSymbols, converted)
    assume(!definedSymbols.contains(newName), s"Symbol $newName already exists in the context")
    (newName, copy(definedSymbols = definedSymbols + newName))
  }
  def addArgument(name: String): (String, TSContext) = {
    val converted = IdentifierRules.convertToJSIdentifier(name)
    val newName = IdentifierRules.newSymbol(definedSymbols, converted)
    assume(!definedSymbols.contains(newName), s"Symbol $newName already exists in the context")
    (newName, copy(definedSymbols = definedSymbols + newName))
  }
  def link(id: UniqidOf[LocalVar], name: String): Unit = {
    assume(!map.contains(id), s"Variable $name already exists in the context")
    map.put(id, name): Unit
  }
}

object TSContext {
  def create(map: mutable.HashMap[UniqidOf[LocalVar], String] = new mutable.HashMap()): TSContext =
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
  def compileType(term: Term)(using TSContext): TSType = term match {
    case IntType(meta)  => NumberType(meta)
    case UIntType(meta) => NumberType(meta)
    case FunctionType(Seq(telescope), returnTy, effects, meta) =>
      require(effects.assumeEffects.isEmpty, "Effects are not supported in TypeScript backend")
      TSFunctionType(
        telescope.args.zipWithIndex.map { case (arg, i) => Param(arg.bind.map(_.name).getOrElse(s"_$i"), compileType(arg.ty)) },
        compileType(returnTy),
        meta
      )
    case NothingType(meta) => NeverType(meta)
    case term              => throw new UnsupportedOperationException(t"This has not been implemented yet: class ${term.getClass} $term")
  }
  def compileExpr(term: Term)(using ctx: TSContext): TSExpr = term match {
    case IntTerm(value, meta)  => DoubleExpr(value.toDouble, meta)
    case UIntTerm(value, meta) => DoubleExpr(value.toDouble, meta)
    case NativeTerm(repr, ty, _, meta) =>
      repr match {
        case StringTerm(code, _) => RawExpr(code, meta)
        case _ =>
          throw new UnsupportedOperationException(
            t"This has not been implemented yet: NativeTerm with repr $repr and type $ty"
          )
      }
    case localV: LocalVar =>
      val name = ctx.map.getOrElse(
        localV.uniqId,
        throw new IllegalArgumentException(s"Variable ${localV.name} not found in context")
      )
      IdentifierExpr(name, localV.meta)
    case f: FCallTerm =>
      f match {
        case FCallTerm(f, Seq(Calling(args, i, cmeta)), meta) => FunctionCallExpr(compileExpr(f), args.map(x => compileExpr(x.value)), meta)
        case other =>
          throw new UnsupportedOperationException(
            t"This has not been implemented yet: FCallTerm multiple telescopes $f"
          )
      }
    case f: Function if f.ty.telescopes.size == 1 =>
      var innerCtx = ctx
      assume(f.ty.effects.assumeEffects.isEmpty, "Effects are not yet supported in TypeScript backend")
      val telescope = f.ty.telescopes.head
      val args = telescope.args.zipWithIndex.map { case (arg, i) =>
        val (name, innerCtx1) = innerCtx.addArgument(arg.bind.map(_.name).getOrElse(s"_$i"))
        innerCtx = innerCtx1
        arg.bind match {
          case Some(localv) =>
            ctx.link(localv.uniqId, name)
          case None =>
        }
        Param(name, compileType(arg.ty)(using innerCtx))
      }
      LambdaExpr(
        args,
        compileExpr(f.body)(using innerCtx),
        f.meta
      )
    case term =>
      throw new UnsupportedOperationException(
        t"This has not been implemented yet: class ${term.getClass} $term"
      )
  }
  def introduceLetVar(let: LetStmtTerm)(using ctx: TSContext): (String, TSContext) = {
    // TODO: handle shadowing and uniqueness and javascript reserved words and javascript reserved symbols and a lot more
    val (name, ctx1) = ctx.convertAndAdd(let.localv.name)
    ctx1.link(let.localv.uniqId, name)
    (name, ctx1)
  }
  def introduceLetVar(let: DefStmtTerm)(using ctx: TSContext): (String, TSContext) = {
    // TODO: handle shadowing and uniqueness and javascript reserved words and javascript reserved symbols and a lot more
    val (name, ctx1) = ctx.convertAndAdd(let.localv.name)
    ctx1.link(let.localv.uniqId, name)
    (name, ctx1)
  }
  def useVar(localV: LocalVar)(using ctx: TSContext): String =
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
    // TODO: corrrect scope rules for def
    case let: DefStmtTerm =>
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
    case stmt =>
      throw new UnsupportedOperationException(
        t"This has not been implemented yet: class ${stmt.getClass} $stmt"
      )
  }
}
