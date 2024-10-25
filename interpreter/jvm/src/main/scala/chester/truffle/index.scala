package chester.truffle

import com.oracle.truffle.api.{CallTarget, TruffleLanguage}
import com.oracle.truffle.api.TruffleLanguage.{Env, ParsingRequest}

trait ChesterContext

@TruffleLanguage.Registration(id = "cst", name = "Chester")
class ChesterLang extends TruffleLanguage[ChesterContext] {
  @throws[Exception]
  override protected def parse(request: ParsingRequest): CallTarget = {
    ???
  }

  override protected def createContext(env: Env): ChesterContext = null
}