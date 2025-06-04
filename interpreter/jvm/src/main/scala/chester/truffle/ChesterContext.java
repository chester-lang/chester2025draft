package chester.truffle;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;
import org.jetbrains.annotations.NotNull;

public final class ChesterContext {
    private static final TruffleLanguage.ContextReference<ChesterContext> REF =
            TruffleLanguage.ContextReference.create(ChesterLang.class);

    /** Retrieve the current language context for the given {@link Node}. */
    public static @NotNull ChesterContext get(@NotNull Node node) {
        return REF.get(node);
    }
    
    final @NotNull Env globalEnv;

    public ChesterContext(@NotNull Env globalEnv) {
        this.globalEnv = globalEnv;
    }

    public ChesterContext() {
        this(new Env(ChesterLang.class));
    }

}