package chester.reduce

import chester.syntax.core.*

/** A reducer that can reduce terms to their normal forms.
  * 
  * IMPORTANT: Always use the passed reducer `r` for recursion, never `this.reduce` directly.
  * This allows other reducers to be composed/wrapped around this one.
  */
trait Reducer {
  def reduce(term: Term)(using ReduceContext, Reducer): Term
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
  /** Check if a term is a type-level computation that should be reduced */
  private def isTypeLevel(term: Term): Boolean = term match {
    case FCallTerm(f, _, _) => f match {
      case Function(FunctionType(_, retTy, _, _), _, _) => retTy match {
        case Type(_, _) => true
        case _ => false
      }
      case _ => false
    }
    case _ => false
  }

  def reduce(term: Term, mode: ReduceMode)(using ctx: ReduceContext, r: Reducer): Term = term match {
    // WHNF terms - return as is
    case t: WHNF => t

    // Block terms - reduce statements and result
    case BlockTerm(statements, result, meta) => {
      val reducedStatements = statements.map { case stmt: StmtTerm =>
        r.reduce(stmt).asInstanceOf[StmtTerm]
      }
      val reducedResult = r.reduce(result)
      BlockTerm(reducedStatements, reducedResult, meta)
    }

    // Function calls - reduce function and arguments
    case FCallTerm(f, args, meta) => {
      val reducedF = r.reduce(f)
      val reducedArgs = args.map(calling =>
        Calling(
          calling.args.map(arg => CallingArgTerm(r.reduce(arg.value), r.reduce(arg.ty), arg.name, arg.vararg, arg.meta)),
          calling.implicitly,
          calling.meta
        )
      )
      reducedF match {
        case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
          // Substitute args into body
          val substitutedBody = telescopes.zip(reducedArgs).foldLeft(body) { case (acc, (telescope, calling)) =>
            telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
              acc.substitute(param.bind, arg.value)
            }
          }
          // Only reduce body for normal mode or type-level functions in type-level mode
          mode match {
            case ReduceMode.Normal => r.reduce(substitutedBody)
            case ReduceMode.TypeLevel => retTy match {
              case Type(_, _) => r.reduce(substitutedBody)
              case _ => substitutedBody
            }
          }
        case _ => FCallTerm(reducedF, reducedArgs, meta)
      }
    }

    // Annotations - reduce the term and type
    case Annotation(term, ty, effects, meta) => {
      val reducedTerm = r.reduce(term)
      val reducedTy = ty.map(r.reduce)
      val reducedEffects = effects // Effects don't need reduction
      reducedTerm match {
        case t: WHNF => t // If the term is already in WHNF, return it
        case _       => Annotation(reducedTerm, reducedTy, reducedEffects, meta)
      }
    }

    // Tuple terms - reduce all values
    case TupleTerm(values, meta) =>
      TupleTerm(values.map(r.reduce), meta)

    // For other cases, leave as is for now
    case other => other
  }

  // Default to normal reduction mode for backward compatibility
  override def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term = 
    reduce(term, ReduceMode.Normal)
}
