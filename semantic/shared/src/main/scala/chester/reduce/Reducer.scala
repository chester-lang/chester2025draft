package chester.reduce

import chester.syntax.core.*

case class ReduceContext()

trait Reducer {
  def reduce(term: Term)(using ReduceContext, Reducer): Term
}

object Reducer {
  def reduce(term: Term)(using ctx:ReduceContext, r: Reducer): Term = r.reduce(term)
}

object NaiveReducer extends Reducer {
  override def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term = term match {
    // WHNF terms - return as is
    case t: WHNF => t 
    
    // Block terms - reduce statements and result
    case BlockTerm(statements, result, meta) => {
      val reducedStatements = statements.map {
        case stmt: StmtTerm => r.reduce(stmt).asInstanceOf[StmtTerm]
      }
      val reducedResult = r.reduce(result)
      BlockTerm(reducedStatements, reducedResult, meta)
    }
    
    // Function calls - reduce function and arguments
    case FCallTerm(f, args, meta) => {
      val reducedF = r.reduce(f)
      val reducedArgs = args.map(calling => 
        Calling(
          calling.args.map(arg => 
            CallingArgTerm(r.reduce(arg.value), r.reduce(arg.ty), arg.name, arg.vararg, arg.meta)
          ),
          calling.implicitly,
          calling.meta
        )
      )
      reducedF match {
        case Function(ty, body, _) =>
          // TODO: Implement function application by substituting args into body
          r.reduce(body)
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
        case _ => Annotation(reducedTerm, reducedTy, reducedEffects, meta)
      }
    }

    // Tuple terms - reduce all values
    case TupleTerm(values, meta) =>
      TupleTerm(values.map(r.reduce), meta)
      
    // For other cases, leave as is for now
    case other => other
  }
}