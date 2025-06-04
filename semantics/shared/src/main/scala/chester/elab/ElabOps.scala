package chester.elab

import chester.error.{Reporter, TyckProblem}
import chester.elab.api.SemanticCollector

import scala.language.implicitConversions

case class ElabOps(reporter: Reporter[TyckProblem], collector: SemanticCollector) extends Reporter[TyckProblem] {
  inline def report(value: TyckProblem): Unit = reporter.report(value)
}

implicit def getCollector(x: ElabOps): SemanticCollector = x.collector
