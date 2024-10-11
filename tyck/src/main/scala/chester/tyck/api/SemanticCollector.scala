package chester.tyck.api

import chester.syntax.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import upickle.default.*
import chester.uniqid.*
import scala.collection.mutable

trait SymbolCollector {
  def referencedOn(expr: Expr): Unit = ()
}

object NoopSymbolCollector extends SymbolCollector {}

trait SemanticCollector {
  // TODO: semantic highlighting?
  def highlightLetDef(expr: Expr): Unit = ()
  def highlightLiteral(expr: Expr): Unit = ()

  def newSymbol(
      call: MaybeVarCall,
      id: UniqIdOf[? <: MaybeVarCall],
      definedOn: Expr,
      localCtx: Any // TODO, nessary for deciding type of the symbol if some type is in local variables
  ): SymbolCollector = NoopSymbolCollector

  def metaFinished(replace: MetaTerm => Term): Unit = ()
}

private implicit inline def rwUniqIDOfVar[T]: ReadWriter[UniqIdOf[? <: MaybeVarCall]] =
  rwUniqIDOf.asInstanceOf[ReadWriter[UniqIdOf[? <: MaybeVarCall]]]

// TODO: handle when call's ty is MetaTerm
case class CollectedSymbol(
    call: MaybeVarCall,
    id: UniqIdOf[? <: MaybeVarCall],
    definedOn: Expr,
    referencedOn: Vector[Expr]
) derives ReadWriter {
  def name: Name = call.name

  def metaFinished(replace: MetaTerm => Term): CollectedSymbol = {
    this.copy(call = call.replaceMeta(replace).asInstanceOf[MaybeVarCall])
  }
}

class VectorSemanticCollector extends SemanticCollector {
  private var builder: mutable.ArrayDeque[CollectedSymbol] =
    new mutable.ArrayDeque[CollectedSymbol]()
  override def newSymbol(
      call: MaybeVarCall,
      id: UniqIdOf[? <: MaybeVarCall],
      definedOn: Expr,
      localCtx: Any
  ): SymbolCollector = {
    val index = builder.length
    builder.append(CollectedSymbol(call, id, definedOn, Vector()))
    assert(builder.length == index + 1)
    new SymbolCollector {
      override def referencedOn(expr: Expr): Unit = {
        val symbol = builder(index)
        builder(index) = symbol.copy(referencedOn = symbol.referencedOn :+ expr)
      }
    }
  }
  def get: Vector[CollectedSymbol] = builder.toVector

  override def metaFinished(replace: MetaTerm => Term): Unit = {
    builder = builder.map(_.metaFinished(replace))
  }
}

object NoopSemanticCollector extends SemanticCollector {}

class UnusedVariableWarningWrapper(x: SemanticCollector) extends SemanticCollector {
  private var unusedVariables: Vector[CollectedSymbol] = Vector()
  override def newSymbol(
      call: MaybeVarCall,
      id: UniqIdOf[? <: MaybeVarCall],
      definedOn: Expr,
      localCtx: Any
  ): SymbolCollector = {
    val symbolCollector = x.newSymbol(call, id, definedOn, localCtx)
    val c = CollectedSymbol(call, id, definedOn, Vector())
    unusedVariables = unusedVariables :+ c
    new SymbolCollector {
      override def referencedOn(expr: Expr): Unit = {
        symbolCollector.referencedOn(expr)
        unusedVariables = unusedVariables.filterNot(_ == c)
      }
    }
  }
  def foreachUnused(f: CollectedSymbol => Unit): Unit =
    unusedVariables.foreach(f)
}
