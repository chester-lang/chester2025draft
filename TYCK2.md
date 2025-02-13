# Type Checker Implementation Plan (tyck2)

## Goal
Support type-level computation during type checking by integrating elaboration with reduction. Example:
```chester
record A(a: Integer);
def idType(x: Type): Type = x;
let aT = A;
def getA(x: A): Integer = x.a;
def getA1(x: aT): Integer = x.a;
```

## Current System
- `Elaborater.scala`: Type checks and elaborates terms, with support for evaluation during checking
- `Reducer.scala`: Reduces terms to normal form, including function application
- System preserves original terms in core representation unless reduction needed

## Implementation Plan

### Step 1: Reduction Integration
Modify type checking to integrate with reduction:
- Add `TyckReducer` extending `Reducer` for type-level evaluation
- Integrate reduction with unification to handle type equality
- Control when terms are reduced vs preserved

Code Changes:
```scala
// 1. Create new TyckReducer.scala
package chester.tyck

class TyckReducer(ctx: Context) extends Reducer {
  override def reduce(term: Term)(using ReduceContext, Reducer): Term = term match {
    case FCallTerm(f, args, meta) if isTypeLevelCall(f) =>
      val reducedF = r.reduce(f)
      reducedF match {
        case Function(FunctionType(telescopes, retTy, _, _), body, _) 
          if isTypeResult(retTy) =>
          super.reduce(term)  // Fully reduce type-level computation
        case _ => term  // Preserve other function calls
      }
    case _ => super.reduce(term)
  }

  private def isTypeLevelCall(f: Term): Boolean = {
    f.ty match {
      case FunctionType(_, retTy, _, _) => isTypeResult(retTy)
      case _ => false
    }
  }

  private def isTypeResult(ty: Term): Boolean = ty match {
    case Type0 | Typeω => true
    case _ => false
  }
}

// 2. Modify Unify propagator in TyckPropagator.scala
case class Unify(left: CellId[Term], right: CellId[Term], cause: Expr) extends Propagator {
  override def propagate(using Context, Tyck, StateAbility[Tyck]): Unit = {
    val leftTerm = state.read(left)
    val rightTerm = state.read(right)
    
    // Add reduction before unification
    val tyckReducer = new TyckReducer(ctx)
    val reducedLeft = tyckReducer.reduce(leftTerm)
    val reducedRight = tyckReducer.reduce(rightTerm)
    
    unifyTerms(reducedLeft, reducedRight, cause)
  }
}
```

Example:
```chester
def idType(x: Type): Type = x;
let aT = idType(A);  // Should reduce to A during type checking
```

### Step 2: Type-Level Let Bindings
Enhance let binding handling:
- Track type-level values in context
- Reduce references to type-level values during checking
- Maintain original terms in core representation

Code Changes:
```scala
// 1. Enhance Context.scala
case class Context(
  // ... existing fields ...
  typeLevelBindings: Map[String, (Term, CellId[Term])] = Map.empty
) {
  def addTypeLevelBinding(name: String, term: Term, ty: CellId[Term]): Context =
    copy(typeLevelBindings = typeLevelBindings + (name -> (term, ty)))
    
  def getTypeLevelBinding(name: String): Option[(Term, CellId[Term])] =
    typeLevelBindings.get(name)
}

// 2. Modify identifier handling in Elaborater.scala
case expr @ Identifier(name, _) => {
  localCtx.get(name) match {
    case Some(c: ContextItem) => /* existing code */
    case None => {
      // Add type-level binding check
      localCtx.getTypeLevelBinding(name) match {
        case Some((term, tyId)) =>
          state.addPropagator(Unify(ty, tyId, expr))
          term
        case None => /* existing fallback code */
      }
    }
  }
}
```

Example:
```chester
let aT = A;
let bT = aT;  // Should resolve to A during type checking
```

### Step 3: Field Access with Type Reduction
Connect field access checking with reduction:
- Reduce type references before field lookup
- Handle field access on reduced type values
- Preserve original type references in core terms

Code Changes:
```scala
// Enhance RecordFieldPropagator in TyckPropagator.scala
case class RecordFieldPropagator(
  recordTy: CellId[Term], 
  fieldName: String,
  resultTy: CellId[Term],
  cause: Expr
) extends Propagator {
  override def propagate(using Context, Tyck, StateAbility[Tyck]): Unit = {
    val tyTerm = state.read(recordTy)
    
    // Add reduction of record type
    val tyckReducer = new TyckReducer(ctx)
    val reducedTy = tyckReducer.reduce(tyTerm)
    
    reducedTy match {
      case RecordType(fields, _) =>
        fields.find(_.name == fieldName) match {
          case Some(field) => 
            state.addPropagator(Unify(resultTy, field.ty, cause))
          case None =>
            state.reporter(FieldNotFound(fieldName, reducedTy, cause))
        }
      case _ =>
        state.reporter(NotARecord(reducedTy, cause))
    }
  }
}
```

Example:
```chester
record A(a: Integer);
let aT = A;
def getA1(x: aT): Integer = x.a;  // Should reduce aT to A for field lookup
```

## Testing Strategy
Test reduction behavior during type checking:

1. Type-level reduction:
```chester
def idType(x: Type): Type = x;
def id2(x: Type): Type = idType(x);
let aT = id2(A);  // Should reduce to A
```

2. Let binding reduction:
```chester
let aT = A;
let bT = aT;
let cT = bT;  // Should all resolve to A
```

3. Field access with reduction:
```chester
record A(a: Integer);
def idType(x: Type): Type = x;
let aT = idType(A);
def getA1(x: aT): Integer = x.a;  // Should work through reduction
``` 