package chester.truffle.node;

import chester.truffle.ChesterContext;
import chester.truffle.ChesterLang;
import com.oracle.truffle.api.nodes.Node;

public abstract class ChesterNode extends Node {
    protected final ChesterLang currentTruffleLanguage() {
        return ChesterLang.get(this);
    }

    /** Allows retrieving the current Truffle language Context from within a Node. */
    protected final ChesterContext currentLanguageContext() {
        return ChesterContext.get(this);
    }
}
