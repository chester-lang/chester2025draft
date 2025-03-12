package chester.tyck

import chester.error.*

// maybe add marks about what item is used when there is a multiple choice. Maybe report some warning when two or more candidates are equally good
case class TyckResult0[Problem <: WithServerity, +S, +T](
    state: S,
    result: T,
    problems: Vector[Problem] = Vector()
) {
  private var noErrors: java.lang.Boolean = null

  def errorsEmpty: Boolean = {
    if (noErrors != null) return noErrors
    val result = !problems.exists(_.isError)
    noErrors = result
    result
  }

  def >>[S, T](next: TyckResult0[Problem, S, T]): TyckResult0[Problem, S, T] = {
    TyckResult0(
      state = next.state,
      result = next.result,
      problems = problems ++ next.problems
    )
  }
}

type TyckResult[+S, +T] = TyckResult0[TyckProblem, S, T]

object TyckResult {
  def apply[S, T](
      state: S,
      result: T,
      warnings: Vector[TyckWarning] = Vector(),
      errors: Vector[TyckError] = Vector()
  ): TyckResult[S, T] = {
    TyckResult0(state, result, warnings ++ errors)
  }

  object Success {
    def unapply[S, T](
        x: TyckResult[S, T]
    ): Option[(T, S, Vector[TyckWarning])] = {
      Option.when(x.errorsEmpty)((x.result, x.state, x.problems.asInstanceOf[Vector[TyckWarning]]))
    }
  }

  object Failure {
    def unapply[S, T](
        x: TyckResult[S, T]
    ): Option[(Vector[TyckError], Vector[TyckWarning], S, T)] = {
      Option.when(!x.errorsEmpty)((
            x.problems
              .collect { case e: TyckError => e },
            x.problems
              .collect { case w: TyckWarning => w },
            x.state,
            x.result
          ))
    }
  }
}
