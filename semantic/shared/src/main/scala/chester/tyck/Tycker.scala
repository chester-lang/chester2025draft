package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.*
import chester.utils.propagator.*

object Tycker extends DefaultImpl {
  def check(expr: Expr): TyckResult[Context, Judge] = {
    given Context = Context.empty
    given SemanticCollector = NoopSemanticCollector
    given Tyck = new Tyck {
      var problems: Vector[TyckProblem] = Vector()
      def reporter(problem: TyckProblem): Unit = {
        problems = problems :+ problem
      }
    }
    given StateAbility[Tyck] = new StateAbility[Tyck] {
      def addPropagator(propagator: Propagator): Unit = {
        propagator match {
          case Unify(expected, actual, cause) =>
            // Handle type unification
            (expected, actual) match {
              case (t1: Type, t2: Type) =>
                // Types are compatible
                ()
              case (t1: TupleType, t2: TupleType) if t1.elements.size == t2.elements.size =>
                // Unify tuple elements
                t1.elements.zip(t2.elements).foreach { case (e1, e2) =>
                  addPropagator(Unify(toId(e1), toId(e2), cause))
                }
              case (t1: RecordType, t2: RecordType) =>
                // Unify record fields
                t1.fields.zip(t2.fields).foreach { case (f1, f2) =>
                  addPropagator(Unify(toId(f1.ty), toId(f2.ty), cause))
                }
              case _ =>
                summon[Tyck].reporter(TypeMismatch(expected, actual, cause))
            }
          case _ => propagator.propagate
        }
      }
    }

    try {
      val ty = newType
      val term = elab(expr, ty, toEffectsCell(Effects.Empty))
      val judge = Judge(term, toTerm(ty))
      TyckResult(summon[Context], judge, Vector(), summon[Tyck].problems)
    } catch {
      case e: TyckError =>
        TyckResult(summon[Context], Judge(ErrorTerm(e, None), ErrorType(None)), Vector(), Vector(e))
    }
  }
}
