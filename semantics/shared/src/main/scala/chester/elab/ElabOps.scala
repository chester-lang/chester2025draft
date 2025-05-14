package chester.elab

import chester.error.{Reporter, TyckProblem}
import chester.tyck.api.SemanticCollector
import scala.language.implicitConversions

case class ElabOps(reporter: Reporter[TyckProblem], collector: SemanticCollector) {}

implicit def getReporter(x: ElabOps): Reporter[TyckProblem] = x.reporter
implicit def getCollector(x: ElabOps): SemanticCollector = x.collector
