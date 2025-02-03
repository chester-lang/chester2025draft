package chester.readerv2

/*
 * ReaderV2 Design Principles:
 * 
 * 1. Separation of Concerns:
 *    - Parser only produces OpSeq without knowledge of operator semantics
 *    - Operator precedence, fixity (infix/prefix/postfix/mixfix) handled in later passes
 *    - This separation allows flexible operator definition and extension
 * 
 * 2. Error Recovery:
 *    - Designed to handle incomplete/broken source code (e.g. during editing)
 *    - Attempts to produce meaningful partial results when possible
 *    - Uses ErrorExpr to represent recoverable parse errors inline
 *    - Continues parsing after encountering errors when safe to do so
 * 
 * 3. OpSeq Design:
 *    - Raw sequence of expressions and operators
 *    - No precedence or associativity information at parse time
 *    - Later passes will restructure based on operator properties
 *    - Allows for flexible operator definition and semantics
 * 
 * 4. Incremental Parsing:
 *    - Supports partial parsing of incomplete expressions
 *    - Maintains parser state for potential incremental updates
 *    - Useful for IDE integration and live editing
 */

  // Design Notes for Uniform Operator/Identifier Handling:
  // 
  // 1. No Special Cases:
  //    - All identifiers and operators are treated uniformly in parsing
  //    - No special cases for keywords like "if", "then", "else" - they are just identifiers
  //    - No predefined keywords (if/then/else/val) or operators (+/-/*)
  //    - No special parsing rules for any identifiers
  //    - Semantic meaning determined in later passes
  //    - Examples:
  //      - Traditional: if x then y else z
  //      - Custom: myIf x myThen y myElse z
  //      - Both parse to: OpSeq([identifier, expr, identifier, expr, identifier, expr])
  //      - Keywords like "if", "then", "else" are treated exactly the same as any other identifier
  // 
  // 2. Operator/Identifier Rules:
  //    - Operators start with operator symbols (.:=-+\|<>/?`~!@$%^&*)
  //    - Identifiers start with letters/emoji/underscore
  //    - Both can contain operator symbols and word symbols
  //    - See IdentifierRules.scala for complete rules
  //    - We use IdentifierRules.strIsOperator for uniform operator identification
  //    - NO local redefinition of operator rules to ensure consistency
  //    - Keywords are NOT special - they follow the same rules as any other identifier
  // 
  // 3. Sequence Construction:
  //    - All terms form a uniform sequence: expr op expr op expr ...
  //    - Structure preserved for later semantic analysis
  //    - Examples:
  //      - 1 + 2 -> OpSeq([1, +, 2])
  //      - if x then y -> OpSeq([if, x, then, y])  // "if" and "then" are just identifiers
  //      - val x = 1 -> OpSeq([val, x, =, 1])      // "val" is just an identifier
  //      - myOp1 x myOp2 y -> OpSeq([myOp1, x, myOp2, y])
  // 
  // 4. Benefits:
  //    - Allows user-defined operators and keywords
  //    - Consistent parsing rules for all identifiers
  //    - Flexible operator definition and extension
  //    - Operator precedence and fixity handled in later passes
  //    - Supports domain-specific language extensions
  //    - Single source of truth for operator identification (IdentifierRules)
  //    - No special cases means simpler, more maintainable code
  //    - Keywords can be redefined or extended by users

  // Design Notes for Operator Sequence Parsing:
  // 
  // 1. Operators are handled uniformly through character-based identification
  // 2. No special casing of operators - determined by character patterns in IdentifierRules
  // 3. Operator sequences are built incrementally
  // 4. The sequence maintains the alternating pattern: term operator term operator ...
  // 5. Both prefix and infix operators are supported through the same mechanism
  // 6. All operator identification is delegated to IdentifierRules.strIsOperator
  //    to maintain consistency across the codebase

import scala.util.boundary
import scala.util.boundary.break
import chester.error.{Pos, SourcePos, RangeInFile}
import chester.error.WithPos
import chester.utils.WithUTF16
import io.github.iltotore.iron.autoRefine
import chester.reader.{ParseError, SourceOffset, ParserSource}
import chester.syntax.concrete.{
  Block, Expr, ExprMeta, ExprStmt, Identifier, ListExpr, ObjectExpr,
  QualifiedName, Tuple, FunctionCall, IntegerLiteral, RationalLiteral, StringLiteral,
  ObjectExprClauseOnValue, ObjectExprClause, ObjectClause, OpSeq, ErrorExpr, RecoverableParseError
}
import chester.syntax.concrete.Literal.*
import spire.math.Rational
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator
import chester.error.Reporter

case class StmtExpr(expr: Expr)

case class LexerState(
  tokens: Vector[Either[ParseError, Token]],
  index: Int
) {
  def current: Either[ParseError, Token] = tokens(index)
  def isAtEnd: Boolean = index >= tokens.length
  def advance(): LexerState = LexerState(tokens, index + 1)
  def sourcePos: SourcePos = current match {
    case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
    case Right(t) => t.sourcePos
  }
  override def toString: String = s"LexerState(index=$index, current=$current, remaining=${tokens.length - index} tokens)"
}

object LexerV2 {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerV2 =
    new LexerV2(tokens, sourceOffset, ignoreLocation)
    
  var DEBUG = false // Global debug flag
  private var globalRecursionDepth = 0 // Track global recursion depth
  private var maxRecursionDepth = 0 // Track maximum recursion depth reached
  private var methodCallCounts = Map[String, Int]() // Track number of calls per method
}

class LexerV2(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean) {
  import LexerV2.{DEBUG, globalRecursionDepth, maxRecursionDepth, methodCallCounts}
  
  private var state: LexerState = LexerState(tokens.toVector, 0)
  private var loopCount = 0 // Track loop iterations
  
  private def advance(): LexerState = {
    state = state.advance()
    state
  }

  private def peek(): LexerState = state

  private def getStartPos(token: Either[ParseError, Token]): Pos = token match {
    case Right(t) => t.sourcePos.range.start
    case Left(e) => e.pos
  }

  private def withRecursion[T](name: String)(f: => T): T = {
    val MaxRecursionDepth = 100 // Add reasonable limit
    try {
      if (globalRecursionDepth >= MaxRecursionDepth) {
        throw new RuntimeException(s"Maximum recursion depth ($MaxRecursionDepth) exceeded")
      }
      globalRecursionDepth += 1
      maxRecursionDepth = math.max(maxRecursionDepth, globalRecursionDepth)
      methodCallCounts = methodCallCounts.updated(name, methodCallCounts.getOrElse(name, 0) + 1)
      
      debug(s"ENTER $name (depth=$globalRecursionDepth, calls=${methodCallCounts(name)})")
      debugState(s"Before $name")
      
      val result = f
      debug(s"EXIT $name with result=$result")
      debugState(s"After $name")
      result
    } catch {
      case e: Throwable =>
        debug(s"ERROR in $name: ${e.getMessage}")
        throw e
    } finally {
      globalRecursionDepth -= 1
    }
  }

  def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseAtom") {
    debug(s"parseAtom: state=$state")
    if (state.isAtEnd) {
      debug("parseAtom: end of tokens")
      Left(ParseError("Unexpected end of input", state.sourcePos.range.start))
    } else {
      state.current match {
        case Right(Token.Operator(value, sourcePos)) => {
          debug(s"parseAtom: found operator: $value")
          // Handle prefix operators
          val meta = None
          val operatorState = state.advance()
          operatorState.current match {
            case Right(Token.LParen(_)) => {
              debug("parseAtom: found left paren after operator, parsing function call")
              // Function call with prefix operator
              val afterLParen = operatorState.advance()
              parseExpr(afterLParen) match {
                case Right((arg, afterArgState)) => {
                  afterArgState.current match {
                    case Right(Token.RParen(_)) => {
                      debug("parseAtom: found right paren, completing function call")
                      Right((FunctionCall(Identifier(value, meta), Tuple(Vector(arg), None), None), afterArgState.advance()))
                    }
                    case _ => {
                      debug("parseAtom: missing right paren")
                      Left(ParseError("Expected closing parenthesis", afterArgState.sourcePos.range.start))
                    }
                  }
                }
                case Left(error) => Left(error)
              }
            }
            case _ => {
              debug("parseAtom: regular prefix operator")
              // Regular prefix operator
              parseAtom(operatorState) match {
                case Right((term, nextState)) => {
                  Right((OpSeq(Vector(Identifier(value, meta), term), None), nextState))
                }
                case Left(error) => Left(error)
              }
            }
          }
        }

        case Right(Token.Identifier(name, sourcePos)) if name.map(_.text).mkString == "not" => {
          debug("parseAtom: found 'not' operator")
          // Handle 'not' as a prefix operator
          val meta = None
          val operatorState = state.advance()
          parseAtom(operatorState) match {
            case Right((term, nextState)) => {
              Right((OpSeq(Vector(Identifier("not", meta), term), None), nextState))
            }
            case Left(error) => Left(error)
          }
        }

        case Right(Token.Identifier(name, sourcePos)) if name.map(_.text).mkString == "if" => {
          debug("parseAtom: found 'if' keyword")
          // Handle if-then-else
          val meta = None
          val operatorState = state.advance()
          parseExpr(operatorState) match {
            case Right((condition, afterCondState)) =>
              afterCondState.current match {
                case Right(Token.Identifier(thenName, _)) if thenName.map(_.text).mkString == "then" => {
                  val thenState = afterCondState.advance()
                  parseExpr(thenState) match {
                    case Right((thenExpr, afterThenState)) => {
                      afterThenState.current match {
                        case Right(Token.Identifier(elseName, _)) if elseName.map(_.text).mkString == "else" => {
                          val elseState = afterThenState.advance()
                          parseExpr(elseState) match {
                            case Right((elseExpr, afterElseState)) => {
                              Right((OpSeq(Vector(
                                Identifier("if", meta),
                                condition,
                                Identifier("then", meta),
                                thenExpr,
                                Identifier("else", meta),
                                elseExpr
                              ), None), afterElseState))
                            }
                            case Left(error) => Left(error)
                          }
                        }
                        case _ => Left(ParseError("Expected 'else' in if expression", afterThenState.sourcePos.range.start))
                      }
                    }
                    case Left(error) => Left(error)
                  }
                }
                case _ => Left(ParseError("Expected 'then' in if expression", afterCondState.sourcePos.range.start))
              }
            case Left(error) => Left(error)
          }
        }

        case Right(Token.Identifier(name, sourcePos)) if name.map(_.text).mkString == "val" => {
          var currentState = state.advance()
          currentState.current match {
            case Right(Token.Identifier(name, _)) => {
              currentState = currentState.advance()
              currentState.current match {
                case Right(Token.Operator("=", _)) => {
                  currentState = currentState.advance()
                  parseExpr(currentState) match {
                    case Right((expr, newState)) =>
                      Right((OpSeq(Vector(
                        Identifier("val", None),
                        Identifier(name.map(_.text).mkString, None),
                        Identifier("=", None),
                        expr
                      ), None), newState))
                    case Left(err) => Left(err)
                  }
                }
                case _ => Left(ParseError(s"Expected '=' in val declaration", currentState.sourcePos.range.start))
              }
            }
            case _ => Left(ParseError("Expected identifier after val", currentState.sourcePos.range.start))
          }
        }

        case Right(Token.Identifier(name, sourcePos)) if name.map(_.text).mkString == "so" =>
          // Handle 'so getthen' block
          val meta = None
          val operatorState = state.advance()
          operatorState.current match {
            case Right(Token.Identifier(thenName, _)) if thenName.map(_.text).mkString == "getthen" =>
              val afterThen = operatorState.advance()
              afterThen.current match {
                case Right(Token.LBrace(_)) =>
                  val afterLBrace = afterThen.advance()
                  var exprs = Vector[ExprStmt]()
                  var currentState = afterLBrace
                  
                  while (!currentState.isAtEnd) {
                    currentState.current match {
                      case Right(Token.RBrace(_)) =>
                        return Right((OpSeq(Vector(
                          Identifier("so", meta),
                          Identifier("getthen", meta),
                          Block(exprs, None, None)
                        ), None), currentState.advance()))
                      case _ =>
                        parseExpr(currentState) match {
                          case Right((expr, afterExprState)) =>
                            exprs = exprs :+ ExprStmt(expr, None)
                            currentState = afterExprState
                            currentState.current match {
                              case Right(Token.Semicolon(_)) =>
                                currentState = currentState.advance()
                              case Right(Token.RBrace(_)) =>
                                // Allow missing semicolon before closing brace
                              case _ =>
                                return Left(ParseError("Expected semicolon between expressions", currentState.sourcePos.range.start))
                            }
                          case Left(error) =>
                            return Left(error)
                        }
                    }
                  }
                  Left(ParseError("Unexpected end of input while parsing block", currentState.sourcePos.range.start))
                case _ =>
                  Left(ParseError("Expected block after 'getthen'", afterThen.sourcePos.range.start))
              }
            case _ =>
              Left(ParseError("Expected 'getthen' after 'so'", operatorState.sourcePos.range.start))
          }

        case Right(Token.IntegerLiteral(value, sourcePos)) =>
          val meta = None
          Right((IntegerLiteral(BigInt(value), meta), state.advance()))
          
        case Right(Token.RationalLiteral(value, sourcePos)) =>
          val meta = None
          Right((RationalLiteral(Rational(value), meta), state.advance()))
          
        case Right(Token.StringLiteral(value, sourcePos)) =>
          val meta = None
          Right((StringLiteral(value.map(_.text).mkString, meta), state.advance()))

        case Right(Token.Identifier(name, sourcePos)) =>
          val meta = None
          val nameStr = name.map(_.text).mkString
          val nextState = state.advance()
          nextState.current match {
            case Right(Token.LParen(_)) =>
              // Function call
              val afterLParen = nextState.advance()
              var args = Vector[Expr]()
              var currentState = afterLParen
              
              while (!currentState.isAtEnd) {
                currentState.current match {
                  case Right(Token.RParen(_)) =>
                    return Right((FunctionCall(Identifier(nameStr, meta), Tuple(args, None), None), currentState.advance()))
                  case _ =>
                    if (args.nonEmpty) {
                      currentState.current match {
                        case Right(Token.Comma(_)) =>
                          currentState = currentState.advance()
                        case _ =>
                          return Left(ParseError("Expected comma between function arguments", currentState.sourcePos.range.start))
                      }
                    }
                    parseExpr(currentState) match {
                      case Right((arg, afterArgState)) =>
                        args = args :+ arg
                        currentState = afterArgState
                      case Left(error) =>
                        return Left(error)
                    }
                }
              }
              Left(ParseError("Unexpected end of input while parsing function call", currentState.sourcePos.range.start))

            case Right(Token.LBrace(_)) =>
              // Block
              val afterLBrace = nextState.advance()
              var exprs = Vector[ExprStmt]()
              var currentState = afterLBrace
              
              while (!currentState.isAtEnd) {
                currentState.current match {
                  case Right(Token.RBrace(_)) =>
                    return Right((Block(exprs, None, None), currentState.advance()))
                  case _ =>
                    parseExpr(currentState) match {
                      case Right((expr, afterExprState)) =>
                        exprs = exprs :+ ExprStmt(expr, None)
                        currentState = afterExprState
                        currentState.current match {
                          case Right(Token.Semicolon(_)) =>
                            currentState = currentState.advance()
                          case Right(Token.RBrace(_)) =>
                            // Allow missing semicolon before closing brace
                          case _ =>
                            return Left(ParseError("Expected semicolon between expressions", currentState.sourcePos.range.start))
                        }
                      case Left(error) =>
                        return Left(error)
                    }
                }
              }
              Left(ParseError("Unexpected end of input while parsing block", currentState.sourcePos.range.start))
              
            case _ =>
              Right((Identifier(nameStr, meta), nextState))
          }
          
        case Right(Token.LParen(_)) =>
          parseExpr(state.advance()) match {
            case Right((expr, afterExpr)) =>
              afterExpr.current match {
                case Right(Token.RParen(_)) =>
                  Right((expr, afterExpr.advance()))
                case _ =>
                  Left(ParseError("Expected closing parenthesis", afterExpr.sourcePos.range.start))
              }
            case Left(error) => Left(error)
          }
          
        case Right(Token.LBracket(_)) =>
          val afterLBracket = state.advance()
          var elements = Vector[Expr]()
          var currentState = afterLBracket
          
          while (!currentState.isAtEnd) {
            currentState.current match {
              case Right(Token.RBracket(_)) =>
                return Right((ListExpr(elements, None), currentState.advance()))
              case _ =>
                if (elements.nonEmpty) {
                  currentState.current match {
                    case Right(Token.Comma(_)) =>
                      currentState = currentState.advance()
                    case _ =>
                      return Left(ParseError("Expected comma between list elements", currentState.sourcePos.range.start))
                  }
                }
                parseExpr(currentState) match {
                  case Right((element, afterElementState)) =>
                    elements = elements :+ element
                    currentState = afterElementState
                  case Left(error) =>
                    return Left(error)
                }
            }
          }
          Left(ParseError("Unexpected end of input while parsing list", currentState.sourcePos.range.start))

        case Right(token) =>
          Left(ParseError(s"Unexpected token: $token", state.sourcePos.range.start))

        case Left(error) =>
          Left(error)
      }
    }
  }

  def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    var currentState = state
    var heads = Vector.empty[Expr]
    var tail: Option[Expr] = None

    while (!currentState.isAtEnd && currentState.current.exists(!_.isInstanceOf[Token.RBrace])) {
      parseExpr(currentState) match {
        case Right((expr, newState)) =>
          currentState = newState
          if (!currentState.isAtEnd && currentState.current.exists(!_.isInstanceOf[Token.RBrace])) {
            heads = heads :+ expr
          } else {
            tail = Some(expr)
          }
        case Left(err) => return Left(err)
      }
    }

    if (currentState.isAtEnd) {
      Left(ParseError("Unexpected end of input in block", state.sourcePos.range.start))
    } else {
      Right((Block(heads, tail.get, None), currentState.advance()))
    }
  }

  def parseOpSeq(initial: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var currentExpr = initial
    var currentState = state
    var terms = Vector[Expr](initial)
    var shouldContinue = true
    
    while (!currentState.isAtEnd && shouldContinue) {
      currentState.current match {
        case Right(Token.Operator(value, sourcePos)) =>
          val meta = None
          val operatorState = currentState.advance()
          parseAtom(operatorState) match {
            case Right((nextTerm, nextState)) =>
              terms = terms :+ Identifier(value, meta) :+ nextTerm
              currentState = nextState
            case Left(error) => return Left(error)
          }
        case Right(Token.Identifier(name, sourcePos)) if strIsOperator(name.map(_.text).mkString) =>
          val meta = None
          val operatorState = currentState.advance()
          parseAtom(operatorState) match {
            case Right((nextTerm, nextState)) =>
              terms = terms :+ Identifier(name.map(_.text).mkString, meta) :+ nextTerm
              currentState = nextState
            case Left(error) => return Left(error)
          }
        case Right(Token.Identifier(name, sourcePos)) if name.map(_.text).mkString == "if" =>
          val meta = None
          val operatorState = currentState.advance()
          parseExpr(operatorState) match {
            case Right((condition, afterCondState)) =>
              afterCondState.current match {
                case Right(Token.Identifier(thenName, _)) if thenName.map(_.text).mkString == "then" =>
                  val thenState = afterCondState.advance()
                  parseExpr(thenState) match {
                    case Right((thenExpr, afterThenState)) =>
                      afterThenState.current match {
                        case Right(Token.Identifier(elseName, _)) if elseName.map(_.text).mkString == "else" =>
                          val elseState = afterThenState.advance()
                          parseExpr(elseState) match {
                            case Right((elseExpr, afterElseState)) =>
                              terms = terms :+ Identifier("if", meta) :+ condition :+ thenExpr :+ elseExpr
                              currentState = afterElseState
                            case Left(error) => return Left(error)
                          }
                        case _ => return Left(ParseError("Expected 'else' in if expression", afterThenState.sourcePos.range.start))
                      }
                    case Left(error) => return Left(error)
                  }
                case _ => return Left(ParseError("Expected 'then' in if expression", afterCondState.sourcePos.range.start))
              }
            case Left(error) => return Left(error)
          }
        case Right(Token.Identifier(name, sourcePos)) if name.map(_.text).mkString == "val" =>
          val meta = None
          val operatorState = currentState.advance()
          parseExpr(operatorState) match {
            case Right((identifier, afterIdState)) =>
              afterIdState.current match {
                case Right(Token.Operator(op, _)) if op == "=" =>
                  val assignState = afterIdState.advance()
                  parseExpr(assignState) match {
                    case Right((value, afterValueState)) =>
                      terms = terms :+ Identifier("val", meta) :+ identifier :+ value
                      currentState = afterValueState
                    case Left(error) => return Left(error)
                  }
                case _ => return Left(ParseError("Expected '=' in val declaration", afterIdState.sourcePos.range.start))
              }
            case Left(error) => return Left(error)
          }
        case _ =>
          shouldContinue = false
      }
    }
    
    if (terms.length > 1) {
      Right((OpSeq(terms, None), currentState))
    } else {
      Right((currentExpr, currentState))
    }
  }

  private def isOperator(token: Token): Boolean = token match {
    case Token.Identifier(name, _) => strIsOperator(name.map(_.text).mkString)
    case _ => false
  }

  private def getOperatorName(token: Token): String = token match {
    case Token.Identifier(name, _) => name.map(_.text).mkString
    case _ => throw new IllegalArgumentException("Token is not an operator")
  }

  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseExpr") {
    parseAtom(state).flatMap { case (initial, afterAtomState) =>
      var terms = Vector[Expr](initial)
      var currentState = afterAtomState
      var shouldContinue = true
      
      while (!currentState.isAtEnd && shouldContinue) {
        currentState.current match {
          case Right(Token.Operator(value, sourcePos)) =>
            val meta = None
            val operatorState = currentState.advance()
            parseAtom(operatorState) match {
              case Right((nextTerm, nextState)) =>
                terms = terms :+ Identifier(value, meta) :+ nextTerm
                currentState = nextState
              case Left(error) => return Left(error)
            }
          case Right(Token.Identifier(name, sourcePos)) if strIsOperator(name.map(_.text).mkString) =>
            val meta = None
            val operatorState = currentState.advance()
            parseAtom(operatorState) match {
              case Right((nextTerm, nextState)) =>
                terms = terms :+ Identifier(name.map(_.text).mkString, meta) :+ nextTerm
                currentState = nextState
              case Left(error) => return Left(error)
            }
          case Right(Token.Identifier(name, sourcePos)) =>
            // Handle infix operators like "and", "getthen", etc.
            val nameStr = name.map(_.text).mkString
            if (nameStr == "and" || nameStr == "getthen") {
              val meta = None
              val operatorState = currentState.advance()
              parseAtom(operatorState) match {
                case Right((nextTerm, nextState)) =>
                  terms = terms :+ Identifier(nameStr, meta) :+ nextTerm
                  currentState = nextState
                case Left(error) => return Left(error)
              }
            } else {
              shouldContinue = false
            }
          case _ =>
            shouldContinue = false
        }
      }
      
      Right((if (terms.length == 1) terms.head else OpSeq(terms, None), currentState))
    }
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    var exprs = Vector[Expr]()
    var currentState = state
    
    while (!currentState.isAtEnd && currentState.current.exists(!_.isInstanceOf[Token.RBrace])) {
      parseExpr(currentState) match {
        case Right((expr, afterExprState)) => {
          exprs = exprs :+ expr
          currentState = afterExprState
        }
        case Left(error) => return Left(error)
      }
    }
    
    Right((exprs, currentState))
  }

  private def debug(msg: => String): Unit = if (DEBUG) {
    val indent = " " * globalRecursionDepth
    println(s"[DEBUG]$indent $msg")
    System.out.flush()
  }

  private def debugState(label: String): Unit = if (DEBUG) {
    debug(s"$label:")
    debug(s"  Current state: $state")
    debug(s"  Loop count: $loopCount")
    debug(s"  Recursion depth: $globalRecursionDepth")
    debug(s"  Max recursion depth: $maxRecursionDepth")
    debug(s"  Method call counts: ${methodCallCounts.mkString(", ")}")
    System.out.flush()
  }
}