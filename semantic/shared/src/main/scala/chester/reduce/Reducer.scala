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
    term match {
      // WHNF terms - return as is
      case t: WHNF => t

      // Field access - reduce the target and try to access field
      case FieldAccessTerm(target, field, fieldType, meta) => {
        // First try type-level reduction of target
        val reducedTarget = r.reduce(target, ReduceMode.TypeLevel)
        if (reducedTarget != target) {
          // If type-level reduction worked, try field access on reduced target
          FieldAccessTerm(reducedTarget, field, fieldType, meta)
        } else {
          // If type-level reduction didn't work, try normal reduction
          val normalReducedTarget = r.reduce(target, ReduceMode.Normal)
          FieldAccessTerm(normalReducedTarget, field, fieldType, meta)
        }
      }

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
        // First try type-level reduction of function and args
        val reducedF = r.reduce(f, mode)
        val reducedArgs = args.map(calling =>
          Calling(
            calling.args.map(arg => CallingArgTerm(r.reduce(arg.value, mode), r.reduce(arg.ty), arg.name, arg.vararg, arg.meta)),
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
            // Try reducing the substituted body in the current mode
            val evaluated = r.reduce(substitutedBody, mode)
            if (evaluated != substitutedBody) {
              evaluated
            } else {
              // If reduction in current mode didn't work, try the other mode
              val otherMode = mode match {
                case ReduceMode.TypeLevel => ReduceMode.Normal
                case ReduceMode.Normal => ReduceMode.TypeLevel
              }
              val otherReduced = r.reduce(substitutedBody, otherMode)
              if (otherReduced != substitutedBody) {
                otherReduced
              } else {
                FCallTerm(reducedF, reducedArgs, meta)
              }
            }
          case _ =>
            // Not a function, try reducing in the other mode
            val otherMode = mode match {
              case ReduceMode.TypeLevel => ReduceMode.Normal
              case ReduceMode.Normal => ReduceMode.TypeLevel
            }
            val otherReduced = r.reduce(term, otherMode)
            if (otherReduced != term) {
              otherReduced
            } else {
              FCallTerm(reducedF, reducedArgs, meta)
            }
        }
      }

      // References - try to resolve and reduce
      case ref: ReferenceCall => {
        val resolved = ctx.resolve(ref)
        if (resolved != ref) {
          r.reduce(resolved, mode)
        } else {
          ref
        }
      }

      // Other terms - try to reduce subterms
      case _ => term
    }
  }
}

