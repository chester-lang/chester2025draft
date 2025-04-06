# Union Types Progress

This document outlines the progress and current state of union types implementation in Chester.

## Current State

We've created a test file `simplest_union.chester.todo` with basic test cases for union types, but none of the implementation work has been completed yet.

## Required Changes

Based on our testing and analysis, the following changes are needed in the codebase:

### 1. Parser & Desalting (`Desalt.scala`)

- **Union Type Parsing**: Update the `desugar` method to handle union type expressions:
  ```scala
  def desugar(expr: Expr): Expr = expr match {
    case OpSeq(seq, meta) =>
      // First check if this is a type annotation sequence (x : Type)
      if (seq.length >= 3 && seq(1).isInstanceOf[Identifier] && seq(1).asInstanceOf[Identifier].name == Const.`:`) {
        // Type annotation - process right part potentially as a union
        val lhs = seq(0)
        val colon = seq(1)
        val rhs = if (seq.length == 3) seq(2) else OpSeq(seq.drop(2), meta)
        val processedRhs = desugar(rhs)
        OpSeq(Vector(lhs, colon, processedRhs), meta)
      } else {
        // Look for pipe operators in the sequence
        val pipeIndices = seq.zipWithIndex.collect { case (Identifier("|", _), idx) =>
          idx
        }

        if (pipeIndices.isEmpty) {
          // No pipe operators found, return the original expression
          // with its components desugared
          OpSeq(seq.map(SimpleDesalt.desugar), meta)
        } else {
          // Found pipe operators, construct a union type

          // Extract the types separated by pipe operators
          val types = scala.collection.mutable.ArrayBuffer[Expr]()
          var currentStart = 0

          for (pipeIdx <- pipeIndices) {
            if (pipeIdx > currentStart) {
              // Add the segment before the pipe
              if (pipeIdx - currentStart == 1) {
                // Single element
                types += seq(currentStart)
              } else {
                // Multiple elements forming a type expression
                types += OpSeq(seq.slice(currentStart, pipeIdx), meta)
              }
            } else if (currentStart == pipeIdx) {
              // Empty segment (e.g., "| Type") - this is invalid
              reporter(NotImplemented(opseq))
              return opseq
            }
            // Move past the pipe
            currentStart = pipeIdx + 1
          }

          // Handle the last segment after the final pipe
          if (currentStart < seq.length) {
            if (seq.length - currentStart == 1) {
              types += seq(currentStart)
            } else {
              types += OpSeq(seq.slice(currentStart, seq.length), meta)
            }
          } else {
            // Empty segment at the end (e.g., "Type |") - this is invalid
            reporter(NotImplemented(opseq))
            return opseq
          }

          // Process each type expression to handle nested unions
          val processedTypes = types.map(t => SimpleDesalt.desugar(t)).toVector

          // Create a UnionTypeExpr with the extracted types
          if (processedTypes.nonEmpty) {
            UnionTypeExpr(processedTypes, meta)
          } else {
            reporter(NotImplemented(opseq))
            opseq
          }
        }
      }
    case _ => expr
  }
  ```

### 2. Type Elaboration (`Elaborater.scala`)

- **Type Compatibility Checking**: Implement a more flexible type compatibility system that can handle different representations of the same type:
  ```scala
  private def isTypeStructurallyCompatible(type1: Term, type2: Term): Boolean {
    // Helper to check if a term is an Integer type in any form
    def isIntegerType(t: Term): Boolean = {
      t match {
        case IntegerType(_) => true
        case _ =>
          val s = t.toString.toLowerCase
          s.contains("integer") || (s.contains("int") && !s.contains("interface"))
      }
    }

    // Helper to check if a term is a String type in any form
    def isStringType(t: Term): Boolean = {
      t match {
        case StringType(_) => true
        case _ =>
          val s = t.toString.toLowerCase
          s.contains("string") && !s.contains("stringbuilder")
      }
    }

    // Check based on the matched types
    (type1, type2) match {
      case (t1, t2) if isIntegerType(t1) && isIntegerType(t2) => true
      case (t1, t2) if isStringType(t1) && isStringType(t2) => true
      // Default: standard equality
      case _ => type1 == type2
    }
  }
  ```

- **Unification Enhancement**: Modify the `unify` method to use structural compatibility:
  ```scala
  override def unify(lhs: Term, rhs: Term, cause: Expr) {
    // Check for structural compatibility first
    if (isTypeStructurallyCompatible(lhs, rhs)) return

    // Proceed with type-level reduction if needed
    val lhsResolved = readVar(DefaultReducer.reduce(lhs, ReduceMode.TypeLevel))
    val rhsResolved = readVar(DefaultReducer.reduce(rhs, ReduceMode.TypeLevel))

    if (isTypeStructurallyCompatible(lhsResolved, rhsResolved)) return

    // Continue with existing logic for other cases
    // ...
  }
  ```

- **Union Type Construction**: Properly handle union type expressions:
  ```scala
  case expr @ UnionTypeExpr(types, meta) =>
    // Elaborate each component type
    val componentTypes = types.map(checkType)

    // Create the union type
    val unionTypes = NonEmptyVector.fromVectorUnsafe(componentTypes)
    val unionType = Union(unionTypes, convertMeta(meta))

    // Set up propagators for unification
    val unionId = toId(unionType).asInstanceOf[CellId[Term]]
    val unionTypeIds = componentTypes.map(toId).toVector.map(_.asInstanceOf[CellId[Term]])

    // Connect union to components
    state.addPropagator(UnionOf(unionId, unionTypeIds, expr))

    // Ensure cell coverage
    state.addPropagator(EnsureCellCoverage(unionId, expr))
    unionTypeIds.foreach(id => state.addPropagator(EnsureCellCoverage(id, expr)))

    // Return the union type
    unionType
  ```

### 3. Type Propagator (`TyckPropagator.scala`)

- **UnionOf Improvements**: Enhance the `UnionOf` propagator to better handle union types:
  ```scala
  final case class UnionOf(
    lhs: CellId[Term],
    componentIds: Vector[CellId[Term]],
    cause: Expr
  )(using Context) extends Propagator[Tyck] {
    // Existing code...

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      // When component types are all defined, create a union type
      val rhsValues = rhsValuesOpt.map(_.get)
      val unionType = Union(rhsValues.assumeNonEmpty, None)

      // Handle various cases for LHS
      // ...
    }

    // Enhance zonking to ensure all cells are properly filled
  }
  ```

- **Add Type Compatibility for Union Types**: Enhance the `tryUnify` method to handle union type compatibility:
  ```scala
  // Add specific handling for union type compatibility
  case (Union(types1, _), Union(types2, _)) =>
    // Check if both unions have compatible types
    types1.forall(t1 => types2.exists(t2 => tryUnifyInternal(t1, t2, depth + 1)))

  // Add specific handling for term to union type compatibility
  case (term, Union(types, _)) =>
    // Check if term is compatible with any type in the union
    types.exists(unionType => tryUnifyInternal(term, unionType, depth + 1))

  case (Union(types, _), term) =>
    // Check if any type in the union is compatible with the term
    types.exists(unionType => tryUnifyInternal(unionType, term, depth + 1))
  ```

### 4. Test Framework Updates

- Modify `common.scala` to recognize `.todo` extensions:
  ```scala
  val inputFiles = Files
    .list(path)
    .iterator()
    .asScala
    .filter(file => {
      val fileName = file.toString
      fileName.endsWith(".chester") || fileName.endsWith(".chester.todo")
    })
    .toSeq
  ```

- Update `FilesTyckTest.scala` to handle `.todo` extensions:
  ```scala
  val fileName = inputFile.getFileName.toString
  val baseName = if (fileName.endsWith(".chester.todo")) {
    fileName.stripSuffix(".chester.todo")
  } else {
    fileName.stripSuffix(".chester")
  }
  ```

## Next Steps

1. Implement the changes outlined above
2. Test with the `simplest_union.chester.todo` file
3. Once basic functionality works, remove the `.todo` suffix and add more comprehensive test cases
4. Document the union type features in user documentation

## References

- Test file: `tests/tyck/simplest_union.chester.todo`
- Main implementation files:
  - `Desalt.scala` (parser changes)
  - `Elaborater.scala` (type checking)
  - `TyckPropagator.scala` (propagators)
  - `FilesTyckTest.scala` (test framework)