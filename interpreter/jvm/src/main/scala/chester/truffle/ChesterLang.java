package chester.truffle;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.TruffleLanguage.ParsingRequest;
import com.oracle.truffle.api.nodes.Node;

@TruffleLanguage.Registration(id = "cst", name = "Chester")
public final class ChesterLang extends TruffleLanguage<ChesterContext> {
    private static final LanguageReference<ChesterLang> REF =
            LanguageReference.create(ChesterLang.class);
    /** Retrieve the current language instance for the given {@link Node}. */
    public static ChesterLang get(Node node) {
        return REF.get(node);
    }
    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        return Utils.parse(request);
    }

    @Override
    protected ChesterContext createContext(Env env) {
        return new ChesterContext();
    }
}