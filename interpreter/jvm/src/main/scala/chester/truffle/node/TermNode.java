package chester.truffle.node;

import chester.syntax.core.TermT;
import com.oracle.truffle.api.frame.VirtualFrame;

public abstract class TermNode extends ChesterNode implements TermT<TermNode> {
    public abstract Object executeGeneric(VirtualFrame frame);
}
