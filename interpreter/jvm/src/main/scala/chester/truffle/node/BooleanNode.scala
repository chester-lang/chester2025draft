package chester.truffle.node

import chester.syntax.core.BooleanTermC
import chester.syntax.core.TermMeta
import com.oracle.truffle.api.frame.VirtualFrame

import scala.Option

final case class BooleanNode(override val value: Boolean, override val meta: Option[TermMeta]) extends LiteralNode with BooleanTermC[TermNode] {
  override def executeGeneric(frame: VirtualFrame): Boolean = value
}
