package chester.build

import ch.epfl.scala.bsp4j.*
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

import java.util.concurrent.CompletableFuture
import scala.beans.BeanProperty

trait ChesterBuildServer {

  /** The build target chester options request is sent from the client to the server to query for the list of compiler options necessary to compile a
    * given list of targets.
    */
  @JsonRequest("buildTarget/chesterOptions")
  def buildTargetChesterOptions(
      params: ChesterOptionsParams
  ): CompletableFuture[ChesterOptionsResult]

  // Define additional Chester-specific methods here
}

class ChesterOptionsParams(
    @BeanProperty var targets: java.util.List[BuildTargetIdentifier]
) {
  def this() = this(null)
}

class ChesterOptionsResult(
    @BeanProperty var items: java.util.List[ChesterOptionsItem]
)

class ChesterOptionsItem(
    @BeanProperty var target: BuildTargetIdentifier,
    @BeanProperty var options: java.util.List[String],
    @BeanProperty var classpath: java.util.List[String],
    @BeanProperty var classDirectory: String
)
