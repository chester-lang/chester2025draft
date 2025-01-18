package chester.tyck

import chester.error._
import chester.error.Problem.Severity
import chester.utils.MutBox
import upickle.default._

trait Reporter[-T] {
  def apply(value: T): Unit
}

object StdErrReporter extends Reporter[Problem] {
  def apply(value: Problem): Unit = {
    println(value)
  }
}

extension [T](reporter: Reporter[T]) {
  def report(xs: Seq[T]): Unit = xs.foreach(reporter.apply)
}

class VectorReporter[T] extends Reporter[T] {
  private val buffer = scala.collection.mutable.ArrayBuffer[T]()

  def apply(value: T): Unit = buffer += value

  def getReports: Vector[T] = buffer.toVector
}

case class SeverityMap(
    error: Boolean,
    goal: Boolean,
    warn: Boolean,
    info: Boolean
) derives ReadWriter

object SeverityMap {
  def Empty: SeverityMap =
    SeverityMap(error = false, goal = false, warn = false, info = false)
}

class ReporterTrackError[T <: Problem](x: Reporter[T]) extends Reporter[T] {
  private var errorVar = false
  private var warnVar = false
  private var goalVar = false
  private var infoVar = false

  def apply(value: T): Unit = {
    x.apply(value)
    if value.severity == Severity.Error then errorVar = true
    if value.severity == Severity.Warning then warnVar = true
    if value.severity == Severity.Goal then goalVar = true
    if value.severity == Severity.Info then infoVar = true
  }

  def hasError: Boolean = errorVar
  def hasWarn: Boolean = warnVar
  def hasGoal: Boolean = goalVar
  def hasInfo: Boolean = infoVar

  def getSeverityMap: SeverityMap = SeverityMap(
    error = errorVar,
    goal = goalVar,
    warn = warnVar,
    info = infoVar
  )
}

class Get[P, S](val reporter: Reporter[P], private val state: MutBox[S]) {
  def getState: S = state.get

  implicit inline def toReporter: Reporter[P] = reporter

  def report(problem: P): Unit = reporter.apply(problem)

  def reportseq(problems: Seq[P]): Unit = problems.foreach(report)

  def updateState(f: S => S): Unit = {
    state.update(f)
  }

  def uncheckedSetState(newState: S): Unit = {
    state.set(newState)
  }

  def updateAndMap[T](f: S => (S, T)): T = {
    state.updateAndMap(f)
  }
}

object Get {
  def run[P <: WithServerity, S, A](
      program: Get[P, S] => A
  )(state: S): TyckResult0[P, S, A] = {
    val reporter = new VectorReporter[P]
    val stateBox = MutBox(state)
    val get = Get(reporter, stateBox)
    val result = program(get)
    TyckResult0(stateBox.get, result, reporter.getReports)
  }
}
