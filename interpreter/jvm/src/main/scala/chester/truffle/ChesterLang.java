package chester.truffle;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.TruffleLanguage.ParsingRequest;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import chester.syntax.core.truffle.*;

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
        return Utils.parse(this, request);
    }

    @Override
    protected ChesterContext createContext(Env env) {
        return new ChesterContext();
    }


    public static class ChesterRootNode  extends RootNode {
        @Child Term body;

        ChesterRootNode(ChesterLang lang, Term body) {
            super(lang, new FrameDescriptor());
            this.body = body;
        }

        @Override
        public Object execute(VirtualFrame frame) {
            return body.executeGeneric(frame);
        }
    }

}