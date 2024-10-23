package chester.utils.io.impl

import chester.utils.io.*
import typings.node.processMod
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

implicit object DefaultSpawn extends Spawn[Future] {
  inline override def spawn(x: => Future[Unit]): Unit = x.recover { e =>
    e.printStackTrace()
    processMod.^.exit(1)
  }
}
