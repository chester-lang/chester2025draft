package chester.reduce

import chester.syntax.core.*

/** A reducer that can reduce terms to their normal forms.
  * 
  * IMPORTANT: Always use the passed reducer `r` for recursion, never `this.reduce` directly.
  * This allows other reducers to be composed/wrapped around this one.
  */
trait Reducer {
  def reduce(term: Term)(using ReduceContext, Reducer): Term = reduce(term, ReduceMode.Normal)
  def reduce(term: Term, mode: ReduceMode)(using ReduceContext, Reducer): Term
}

/** Controls how aggressively terms are reduced */
enum ReduceMode {
  case TypeLevel // Only reduce type-level computations
  case Normal    // Normal reduction strategy
}

object Reducer {
  def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term = r.reduce(term)
}

object NaiveReducer extends Reducer {
  override def reduce(term: Term, mode: ReduceMode)(using ctx: ReduceContext, r: Reducer): Term = {
    def tryTypeLevel(t: Term): Option[Term] = {
      val reduced = r.reduce(t, ReduceMode.TypeLevel)
      if (reduced != t) Some(reduced) else None
    }

    def tryNormal(t: Term): Option[Term] = {
      val reduced = r.reduce(t, ReduceMode.Normal)
      if (reduced != t) Some(reduced) else None
    }

    def tryAggressiveReduction(t: Term): Option[Term] = {
      // Try type-level first
      tryTypeLevel(t).orElse {
        // Try normal if type-level fails
        tryNormal(t).flatMap { normalReduced =>
          // Try type-level again on the normal-reduced term
          tryTypeLevel(normalReduced).orElse(Some(normalReduced))
        }
      }
    }

    term match {
      // WHNF terms - return as is
      case t: WHNF => t

      // References - try aggressive resolution and reduction
      case ref: ReferenceCall => {
        val resolved = ctx.resolve(ref)
        if (resolved != ref) {
          tryAggressiveReduction(resolved).getOrElse(resolved)
        } else {
          ref
        }
      }

      // Function calls - aggressive reduction strategy
      case FCallTerm(f, args, meta) => {
        // First try to resolve and reduce the function aggressively
        val reducedF = tryAggressiveReduction(f).getOrElse(f)
        
        // Reduce args aggressively in current mode
        val reducedArgs = args.map(calling =>
          Calling(
            calling.args.map(arg => CallingArgTerm(
              tryAggressiveReduction(arg.value).getOrElse(arg.value),
              tryAggressiveReduction(arg.ty).getOrElse(arg.ty),
              arg.name,
              arg.vararg,
              arg.meta
            )),
            calling.implicitly,
            calling.meta
          )
        )

        // Try to evaluate the function with reduced components
        val evaluatedTerm = reducedF match {
          case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
            // Substitute args into body
            val substitutedBody = telescopes.zip(reducedArgs).foldLeft(body) { case (acc, (telescope, calling)) =>
              telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
                acc.substitute(param.bind, arg.value)
              }
            }
            
            // Try aggressive reduction on the substituted body
            tryAggressiveReduction(substitutedBody).getOrElse(substitutedBody)
          
          case _ => FCallTerm(reducedF, reducedArgs, meta)
        }

        // If evaluation produced a different term, try reducing it again
        if (evaluatedTerm != term) {
          tryAggressiveReduction(evaluatedTerm).getOrElse(evaluatedTerm)
        } else {
          evaluatedTerm
        }
      }

      // Field access - multi-phase reduction strategy
      case FieldAccessTerm(record, field, fieldType, meta) => {
        // Try aggressive reduction on the record
        val reducedRecord = tryAggressiveReduction(record).getOrElse(record)
        
        // If reduction worked, try field access on reduced record
        if (reducedRecord != record) {
          r.reduce(FieldAccessTerm(reducedRecord, field, fieldType, meta), mode)
        } else {
          // Handle different record types
          reducedRecord match {
            case Type(level, _) =>
              // Try aggressive reduction on the level
              val reducedLevel = tryAggressiveReduction(level).getOrElse(level)
              if (reducedLevel != level) {
                r.reduce(FieldAccessTerm(Type(reducedLevel, meta), field, fieldType, meta), mode)
              } else {
                term
              }
            
            case fcall: FCallTerm =>
              // Try aggressive reduction on the function call
              tryAggressiveReduction(fcall).map { reduced =>
                r.reduce(FieldAccessTerm(reduced, field, fieldType, meta), mode)
              }.getOrElse(term)
            
            case ref: ReferenceCall =>
              // Try to resolve and reduce the reference
              val resolved = ctx.resolve(ref)
              if (resolved != ref) {
                r.reduce(FieldAccessTerm(resolved, field, fieldType, meta), mode)
              } else {
                term
              }
            
            case _ => term
          }
        }
      }

      // Block terms - reduce statements and result
      case BlockTerm(statements, result, meta) => {
        val reducedStatements = statements.map { case stmt: StmtTerm =>
          tryAggressiveReduction(stmt).getOrElse(stmt).asInstanceOf[StmtTerm]
        }
        val reducedResult = tryAggressiveReduction(result).getOrElse(result)
        BlockTerm(reducedStatements, reducedResult, meta)
      }

      // Other terms - try aggressive reduction
      case _ => tryAggressiveReduction(term).getOrElse(term)
    }
  }
}

