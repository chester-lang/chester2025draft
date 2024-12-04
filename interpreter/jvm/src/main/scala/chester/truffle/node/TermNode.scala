package chester.truffle.node

import chester.eval.Eval
import chester.syntax.core.TermT
import com.oracle.truffle.api.frame.VirtualFrame

abstract class TermNode extends ChesterNode with TermT[TermNode] {
  def executeGeneric(frame: VirtualFrame): Any = {
    val eval = Eval[TermNode]()
    eval.evalNoEffect(this)
  }
}
