package chester.truffle.node

import chester.syntax.core.LiteralTermT

abstract class LiteralNode extends TermNode with LiteralTermT[TermNode] {}
