package chester.truffle;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.TruffleLanguage.ParsingRequest;

@TruffleLanguage.Registration(id = "cst", name = "Chester")
public class ChesterLang extends TruffleLanguage<ChesterContext> {
    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    @Override
    protected ChesterContext createContext(Env env) {
        return null;
    }
}