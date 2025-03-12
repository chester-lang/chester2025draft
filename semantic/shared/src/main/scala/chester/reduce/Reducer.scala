package chester.reduce

import chester.syntax.core.*
import chester.utils.Debug

/** A reducer that can reduce terms to their normal forms.
  *
  * IMPORTANT: Always use the passed reducer `r` for recursion, never `this.reduce` directly. This allows other reducers to be composed/wrapped around
  * this one.
  */
trait Reducer {
  def reduce(term: Term)(using ReduceContext, Reducer): Term
}

/** Controls how aggressively terms are reduced */
enum ReduceMode {
  case TypeLevel // Only reduce type-level computations
  case Normal // Normal reduction strategy
}

object Reducer {
  def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term = r.reduce(term)
}

object NaiveReducer extends Reducer {

  /** Check if a term is a type-level computation that should be reduced */
  private def isTypeLevel(term: Term): Boolean = term match {
    case FCallTerm(f, _, _) =>
      f match {
        case Function(FunctionType(_, retTy, _, _), _, _) =>
          retTy match {
            case Type(_, _) => true
            case _          => false
          }
        case _ => false
      }
    case _ => false
  }

  /** Helper method for proper reduction of type structures. This ensures consistent handling of types, especially for dependent type systems.
    */
  private def reduceTypeStructure(term: Term)(using ctx: ReduceContext, r: Reducer): Term = {
    import Debug.DebugCategory

    Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Processing term: $term")

    term match {
      case Union(types, meta) =>
        Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Processing Union with types: $types")
        val reducedTypes = types.map(ty => reduceTypeStructure(r.reduce(ty)))
        Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Reduced Union types: $reducedTypes")
        Union(reducedTypes, meta)

      case Intersection(types, meta) =>
        Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Processing Intersection with types: $types")
        val reducedTypes = types.map(ty => reduceTypeStructure(r.reduce(ty)))
        Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Reduced Intersection types: $reducedTypes")
        Intersection(reducedTypes, meta)

      case fcall: FCallTerm if isTypeLevel(fcall) =>
        Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Processing type-level function call: $fcall")
        // First reduce normally
        val reduced = reduceStandard(fcall, ReduceMode.TypeLevel)
        Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Standard reduced result: $reduced")

        // Then check if the result needs further type structure handling
        reduced match {
          // If still a complex type after reduction, process it recursively
          case Union(_, _) | Intersection(_, _) =>
            Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Further reducing complex type: $reduced")
            val result = reduceTypeStructure(reduced)
            Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Final result: $result")
            result
          case _ =>
            Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: Final result: $reduced")
            reduced
        }

      // Other terms are handled by standard reduction
      case _ =>
        Debug.debugPrint(DebugCategory.Reducer, s"reduceTypeStructure: No special handling for term: $term")
        term
    }
  }

  /** Standard reduction logic for terms */
  private def reduceStandard(term: Term, mode: ReduceMode)(using ctx: ReduceContext, r: Reducer): Term = term match {
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
            case ReduceMode.TypeLevel =>
              retTy match {
                case Type(_, _) => r.reduce(substitutedBody)
                case _          => substitutedBody
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

  def reduce(term: Term, mode: ReduceMode)(using ctx: ReduceContext, r: Reducer): Term = {
    // First, apply standard reduction
    val standardReduced = reduceStandard(term, mode)

    // For type-level mode, apply additional type structure handling
    mode match {
      case ReduceMode.TypeLevel =>
        // In type-level mode, ensure complex type structures are properly handled
        reduceTypeStructure(standardReduced)
      case ReduceMode.Normal =>
        // In normal mode, just use the standard reduction
        standardReduced
    }
  }

  // Default to normal reduction mode for backward compatibility
  override def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term =
    reduce(term, ReduceMode.Normal)
}
