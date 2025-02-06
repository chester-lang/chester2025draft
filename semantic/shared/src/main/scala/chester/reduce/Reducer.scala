package chester.reduce

import chester.syntax.core.*

trait Reducer {
  def reduce(term: Term)(using ReduceContext, Reducer): Term
}

object Reducer {
  def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term = r.reduce(term)
}

object NaiveReducer extends Reducer {
  override def reduce(term: Term)(using ctx: ReduceContext, r: Reducer): Term = term match {
    // Block terms - reduce statements and result
    case t: BlockTermC[Term] => {
      val reducedStatements = t.statements.map(stmt => r.reduce(stmt))
      val reducedResult = r.reduce(t.result)
      t.cons.newBlockTerm(reducedStatements.asInstanceOf[Vector[StmtTermT[Term]]], reducedResult, t.meta)
    }

    // Function calls - reduce function and arguments
    case t: FCallTermC[Term] => {
      val reducedF = r.reduce(t.f)
      val reducedArgs = t.args.map(calling =>
        Calling(
          calling.args.map(arg => CallingArgTerm(r.reduce(arg.value), r.reduce(arg.ty), arg.name, arg.vararg, arg.meta)),
          calling.implicitly,
          calling.meta
        )
      )
      reducedF match {
        case f: FunctionC[Term] =>
          val ty = f.ty match {
            case ft: FunctionTypeC[Term] => ft
            case _ => return t.cons.newFCallTerm(reducedF, reducedArgs, t.meta)
          }
          // Substitute args into body
          val substitutedBody = ty.telescope.zip(reducedArgs).foldLeft(f.body) { case (acc, (telescope, calling)) =>
            telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
              acc.substitute(param.bind, arg.value)
            }
          }
          // Reduce the substituted body
          r.reduce(substitutedBody)
        case _ => t.cons.newFCallTerm(reducedF, reducedArgs, t.meta)
      }
    }

    // Annotations - reduce the term and type
    case t: AnnotationC[Term] => {
      val reducedTerm = r.reduce(t.term)
      val reducedTy = t.ty.map(r.reduce)
      val reducedEffects = t.effects // Effects don't need reduction
      reducedTerm match {
        case t: WHNFT[Term] => t // If the term is already in WHNF, return it
        case _ => t.cons.newAnnotation(reducedTerm, reducedTy, reducedEffects, t.meta)
      }
    }

    // Type-level computation
    case t: FieldAccessTermC[Term] => {
      val reducedRecord = r.reduce(t.record)
      reducedRecord match {
        case rec: RecordStmtTermC[Term] =>
          rec.fields.find(_.name == t.fieldName).map(_.ty).getOrElse(t.cons.newFieldAccessTerm(reducedRecord, t.fieldName, t.fieldType, t.meta))
        case _ => t.cons.newFieldAccessTerm(reducedRecord, t.fieldName, t.fieldType, t.meta)
      }
    }

    case t: ObjectCallTermC[Term] =>
      r.reduce(t.objectRef) match {
        case objectDef: ObjectStmtTermC[Term] => objectDef
        case other => t.cons.newObjectCallTerm(other, t.meta)
      }

    case t: RecordCallTermC[Term] =>
      t.cons.newRecordCallTerm(t.recordDef, t.telescope, t.meta)

    // Tuple terms - reduce all values
    case t: TupleTermC[Term] =>
      t.cons.apply(t.values.map(r.reduce), t.meta)

    // WHNF terms - return as is
    case t: WHNFT[Term] => t

    // For other cases, leave as is for now
    case other => other
  }
}
