package chester.truffle;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObject;

public final class ChesterContext {
    private static final TruffleLanguage.ContextReference<ChesterContext> REF =
            TruffleLanguage.ContextReference.create(ChesterLang.class);

    /** Retrieve the current language context for the given {@link Node}. */
    public static ChesterContext get(Node node) {
        return REF.get(node);
    }

}