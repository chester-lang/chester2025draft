package chester.truffle.node;

import chester.syntax.core.BooleanTerm;
import chester.syntax.core.BooleanTermC;
import chester.syntax.core.TermMeta;
import scala.None;
import scala.None$;
import scala.Option;

public final class BooleanNode extends LiteralNode implements BooleanTermC<TermNode> { 
    private final boolean value;

    public BooleanNode(boolean value) {
        this.value = value;
    }
    @Override
    public boolean value() {
        return value;
    }

    @Override
    public Option<TermMeta> meta() {
        return (Option) None$.MODULE$;
    }
}
