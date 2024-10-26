package chester.truffle;

import chester.tyck.Context;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;

@ExportLibrary(InteropLibrary.class)
public class Env implements TruffleObject {
    final @NotNull Class<? extends TruffleLanguage<?>> language;
    final @Nullable Context context;
    public Env(@NotNull Class<? extends TruffleLanguage<?>> language, @Nullable Context context) {
        this.context = context;
        this.language = language;
    }
    public Env(Class<? extends TruffleLanguage<?>> language) {
        this(language, null);
    }
    public @NotNull Context getContext() {
        if(context == null) {
            throw new RuntimeException("Context is not set");
        }
        return context;
    }


    @ExportMessage
    boolean hasLanguage() {
        return true;
    }

    @ExportMessage
    Class<? extends TruffleLanguage<?>> getLanguage() {
        return language;
    }

    @ExportMessage
    @TruffleBoundary
    Object toDisplayString(boolean allowSideEffects) {
        return "#<environment>";
    }
}
