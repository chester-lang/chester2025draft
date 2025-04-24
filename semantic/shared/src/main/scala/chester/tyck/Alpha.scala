package chester.tyck
import chester.syntax.core.*
import chester.utils.*

import scala.language.implicitConversions

trait Alpha extends ElaboraterCommon {

  /** Checks if two terms are alpha-equivalent.
    *
    * Alpha-equivalence is a concept from lambda calculus that considers terms equivalent if they are identical up to consistent renaming of bound
    * variables. This is crucial for dependent type systems where types can contain terms and binding structure matters.
    *
    * Examples:
    *   - (x: Type) -> x and (y: Type) -> y are alpha-equivalent
    *   - (x: Type) -> (y: x) -> y and (a: Type) -> (b: a) -> b are alpha-equivalent
    *
    * @param lhs
    *   First term to compare
    * @param rhs
    *   Second term to compare
    * @param boundVars
    *   Mapping between bound variables in lhs and rhs
    * @return
    *   true if terms are alpha-equivalent, false otherwise
    */
  protected def areAlphaEquivalent(
      lhs: Term,
      rhs: Term,
      boundVars: Map[LocalV, LocalV] = Map.empty
  )(using
    StateAbility[TyckSession],
    Context
  ): Boolean =
    // For alpha-equivalence, we need to check if terms are convertible
    // with respect to bound variable names (alpha conversion)
    (lhs, rhs) match {
      // Check if one term is a bound variable with a mapping to the other term
      case (lv1: LocalV, lv2: LocalV) if boundVars.contains(lv1) =>
        boundVars(lv1) == lv2

      // For function types in a dependent type system, need to
      // be careful with variable bindings in the result type
      case (FunctionType(params1, result1, effects1, _), FunctionType(params2, result2, effects2, _)) =>
        if (params1.length != params2.length) false
        else if (effects1 != effects2) false
        else {
          // We need to build up a mapping between parameter variables
          var updatedBoundVars = boundVars

          // Check that telescopes have equivalent types
          val paramsEqual = params1.lazyZip(params2).forall { (p1, p2) =>
            // Check telescope parameters are equivalent
            if (p1.args.length != p2.args.length) false
            else {
              p1.args.lazyZip(p2.args).forall { (arg1, arg2) =>
                // For each argument, add the binding and check that the types are alpha-equivalent
                val typesEqual = areAlphaEquivalent(arg1.ty, arg2.ty, updatedBoundVars)

                // Update the bound vars mapping with this parameter
                // Use only if arg1.bind is a LocalV
                if (typesEqual) {
                  // Only attempt to update the mapping if we have LocalV values
                  updatedBoundVars = updatedBoundVars
                  true
                } else {
                  false
                }
              }
            }
          }

          if (!paramsEqual) false
          else {
            // For the result type, need to consider bindings
            areAlphaEquivalent(result1, result2, updatedBoundVars)
          }
        }

      // Types with internal structure need recursive checks
      case (Union(types1, _), Union(types2, _)) =>
        typesEquivalentModuloOrdering(types1, types2, boundVars)

      case (Intersection(types1, _), Intersection(types2, _)) =>
        typesEquivalentModuloOrdering(types1, types2, boundVars)

      // For other cases, fall back to regular equality check
      case _ => lhs == rhs
    }

  /** Check if two collections of types are equivalent regardless of their ordering. For union and intersection types, the order doesn't matter.
    *
    * This is important for union and intersection types where:
    *   - A | B is equivalent to B | A
    *   - A & B is equivalent to B & A
    *
    * @param types1
    *   First collection of types
    * @param types2
    *   Second collection of types
    * @param boundVars
    *   Mapping between bound variables
    * @return
    *   true if collections are equivalent modulo ordering, false otherwise
    */
  private def typesEquivalentModuloOrdering(
      types1: Vector[Term],
      types2: Vector[Term],
      boundVars: Map[LocalV, LocalV] = Map.empty
  )(using
    StateAbility[TyckSession],
    Context
  ): Boolean = {
    // For union/intersection types, each type in one collection
    // must have an equivalent in the other collection
    if (types1.length != types2.length) return false

    // Check that each type in types1 has a match in types2
    types1.forall(t1 => types2.exists(t2 => areAlphaEquivalent(t1, t2, boundVars))) &&
    // Check that each type in types2 has a match in types1
    types2.forall(t2 => types1.exists(t1 => areAlphaEquivalent(t1, t2, boundVars)))
  }

}
