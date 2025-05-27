package chester.elab

class MutableContext(var ctx: Context) {
  def update(f: Context => Context): Unit =
    ctx = f(ctx)
}

given mutL(using m: MutableContext): Context = m.ctx
implicit def mutLc(m: MutableContext): Context = m.ctx
