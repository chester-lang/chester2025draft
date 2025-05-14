package chester.elab

import chester.syntax.core.{IntType, IntegerType, ListType, StringType, SymbolType, Term, UIntType}

def eqType(a: Term, b: Term): Boolean = (a, b) match {
  case (IntType(_), IntType(_))         => true
  case (IntegerType(_), IntegerType(_)) => true
  case (UIntType(_), UIntType(_))       => true
  case (StringType(_), StringType(_))   => true
  case (ListType(a, _), ListType(b, _)) => eqType(a, b)
  case (SymbolType(_), SymbolType(_))   => true
  case _                                => a == b
}
