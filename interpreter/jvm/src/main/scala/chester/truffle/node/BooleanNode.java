package chester.truffle.node;

import chester.syntax.core.BooleanTermC;
import chester.syntax.core.TermMeta;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import scala.Option;

@NodeInfo(shortName = "bool")
public final class BooleanNode extends LiteralNode implements BooleanTermC<TermNode> {
    final boolean value;
    final Option<TermMeta> meta;

    public BooleanNode(boolean value, Option<TermMeta> meta) {
        this.value = value;
        this.meta = meta;
    }

    @Override
    public boolean value() {
        return value;
    }

    @Override
    public Option<TermMeta> meta() {
        return meta;
    }
}
