package chester.truffle.node

import chester.syntax.TreeMap
import chester.syntax.core.Term
import chester.syntax.core.TermT
import com.oracle.truffle.api.frame.VirtualFrame
import scala.Function1

abstract class TermNode extends ChesterNode with TermT[TermNode] {
  override final type RootTree = TermNode
  def descent(f: TermNode => TermNode, g: TreeMap[TermNode]): TermNode
  def executeGeneric(frame: VirtualFrame): AnyRef
}
