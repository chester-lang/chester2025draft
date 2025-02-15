package chester.reduce

import chester.tyck.{Context, TyAndVal}
import chester.syntax.core.{Term, TypeDefinition, ReferenceCall}
import chester.uniqid.UniqidOf

/** Context for reduction/evaluation of terms.
  *
  * In type checking, reduction is used to evaluate type-level functions and terms when necessary for type checking (e.g. to check field access on a
  * type constructed by a type function). However, we preserve the original unreduced terms in the core representation unless explicitly requested.
  *
  * This separation allows us to:
  *   1. Control when reduction happens during type checking
  *   2. Preserve the original term structure in the core representation
  *   3. Add additional context needed for reduction in the future
  */
case class ReduceContext(
    knownMap: Map[UniqidOf[ReferenceCall], TyAndVal] = Map.empty,
    typeDefinitions: Map[UniqidOf[TypeDefinition], TypeDefinition] = Map.empty
) {
  /** Resolve a reference to its known value if available */
  def resolve(ref: ReferenceCall): Term = {
    knownMap.get(ref.uniqId.asInstanceOf[UniqidOf[ReferenceCall]]) match {
      case Some(tv: TyAndVal) => tv.value
      case None => ref
    }
  }
}

object ReduceContext {

  /** Convert from tyck Context to ReduceContext. This allows us to use reduction during type checking without explicitly creating a ReduceContext
    * each time.
    */
  given Conversion[Context, ReduceContext] = ctx => ReduceContext(
    knownMap = ctx.knownMap,
    typeDefinitions = ctx.typeDefinitions
  )
}
