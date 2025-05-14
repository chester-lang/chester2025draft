package chester.resolve

import chester.error.*
import chester.syntax.*
import chester.syntax.accociativity.*
import chester.syntax.concrete.*
import scalax.collection.edges.DiEdge
import scalax.collection.immutable.Graph

import scala.collection.mutable

// Constructs the precedence graph from the given context
def constructPrecedenceGraph(
    ctx: PrecedenceGroupCtx
): Graph[PrecedenceGroup, DiEdge[PrecedenceGroup]] = {
  val groups: Seq[PrecedenceGroup] = ctx.groups

  // Create directed edges based on 'higherThan' and 'lowerThan' relationships
  val edges: Seq[DiEdge[PrecedenceGroup]] = groups.flatMap { group =>
    val higherEdges = group.higherThan.map(higherGroup => DiEdge(group, higherGroup))

    val lowerEdges = group.lowerThan.map(lowerGroup => DiEdge(lowerGroup, group))

    higherEdges ++ lowerEdges
  }

  // Construct the graph from the nodes and edges
  val graph = Graph.from(nodes = groups, edges = edges)

  // Check for cycles in the graph
  if (graph.isCyclic) {
    throw new IllegalArgumentException("The precedence graph contains cycles")
  }

  graph
}

// Determines the precedence of an operator based on its group
def precedenceOf(
    opInfo: OpInfo,
    groupPrecedence: Map[QualifiedIDString, Int],
    reporter: Reporter[TyckError]
): Int =
  opInfo match {
    case op: OpWithGroup =>
      val groupName = op.group.name
      groupPrecedence.getOrElse(
        groupName, {
          // Report unknown precedence group
          reporter.report(UnknownPrecedenceGroup(op.group))
          Int.MaxValue
        }
      )
    case _ =>
      // Assign default precedence for operators without a group
      groupPrecedence.getOrElse(DefaultPrecedenceGroup.name, Int.MaxValue)
  }

// Determines the associativity of an operator
def associativityOf(opInfo: OpInfo): Associativity =
  opInfo match {
    case op: OpWithGroup => op.group.associativity
    case _               => Associativity.None
  }

// Determines the operator type (Infix, Prefix, Postfix, Operand) based on context
def determineOpType(
    tokenInfo: TokenInfo,
    prevToken: Option[TokenInfo],
    nextToken: Option[TokenInfo]
): OpType = {
  val isPrevOperand =
    prevToken.exists(_.possibleOpTypes.contains(OpType.Operand))
  val isNextOperand =
    nextToken.exists(_.possibleOpTypes.contains(OpType.Operand))

  val possibleTypes = tokenInfo.possibleOpTypes

  if (possibleTypes.contains(OpType.Infix) && isPrevOperand && isNextOperand) {
    OpType.Infix
  } else if (possibleTypes.contains(OpType.Prefix) && !isPrevOperand && isNextOperand) {
    OpType.Prefix
  } else if (possibleTypes.contains(OpType.Postfix) && isPrevOperand && !isNextOperand) {
    OpType.Postfix
  } else if (possibleTypes.contains(OpType.Operand)) {
    OpType.Operand
  } else {
    // Default to Operand if unable to determine
    OpType.Operand
  }
}

// Parses tokens from expressions, associating them with operator information
def parseTokens(
    seq: Vector[Expr],
    opContext: OperatorsContext,
    groupPrecedence: Map[QualifiedIDString, Int],
    reporter: Reporter[TyckError]
): Vector[TokenInfo] =
  seq.map {
    case id @ Identifier(name, _) =>
      val possibleOps = Seq(
        opContext.resolveInfix(name).map(op => (op, OpType.Infix)),
        opContext.resolvePrefix(name).map(op => (op, OpType.Prefix)),
        opContext.resolvePostfix(name).map(op => (op, OpType.Postfix))
      ).flatten

      if (possibleOps.nonEmpty) {
        val possibleOpTypes = possibleOps.map(_._2).toSet
        val precedences = possibleOps.map { case (opInfo, _) =>
          precedenceOf(opInfo, groupPrecedence, reporter)
        }
        val associativities = possibleOps.map { case (opInfo, _) =>
          associativityOf(opInfo)
        }
        // For simplicity, take the lowest precedence and the first associativity
        val precedence = precedences.min
        val associativity = associativities.head

        TokenInfo(id, precedence, associativity, possibleOpTypes, possibleOps)
      } else {
        // If no operator is found, treat it as an operand
        reporter.report(UnknownOperator(id))
        TokenInfo(id, Int.MaxValue, Associativity.None, Set(OpType.Operand))
      }
    case expr =>
      // Non-identifier expressions are treated as operands
      TokenInfo(expr, Int.MaxValue, Associativity.None, Set(OpType.Operand))
  }

// Builds the expression tree from the output stack using operator information
def buildExpr(
    stack: mutable.Stack[TokenInfo],
    opContext: OperatorsContext,
    reporter: Reporter[TyckError]
): Expr =
  if (stack.isEmpty) {
    reporter.report(UnexpectedTokens(List.empty))
    Identifier("error", meta = None)
  } else {
    val tokenInfo = stack.pop()
    val expr = tokenInfo.expr
    val opType = tokenInfo.possibleOpTypes.headOption.getOrElse(OpType.Operand)

    opType match {
      case OpType.Infix =>
        // For infix operators, pop two operands
        val right = buildExpr(stack, opContext, reporter)
        val left = buildExpr(stack, opContext, reporter)
        val associativity = tokenInfo.associativity
        InfixExpr(left, expr.asInstanceOf[Identifier], right, associativity, expr.meta)
      case OpType.Prefix =>
        // For prefix operators, pop one operand
        val operand = buildExpr(stack, opContext, reporter)
        PrefixExpr(expr.asInstanceOf[Identifier], operand, expr.meta)
      case OpType.Postfix =>
        // For postfix operators, pop one operand
        val operand = buildExpr(stack, opContext, reporter)
        PostfixExpr(operand, expr.asInstanceOf[Identifier], expr.meta)
      case OpType.Operand =>
        // Operands are returned as is
        expr
    }
  }

// Determines whether to pop operators from the stack based on precedence and associativity
def shouldPopOperator(
    topOperator: TokenInfo,
    currentPrec: Int,
    currentAssoc: Associativity
): Boolean = {
  val topPrec = topOperator.precedence
  if (currentAssoc == Associativity.Left) {
    topPrec <= currentPrec
  } else {
    topPrec < currentPrec
  }
}

// Parses the expression tokens into an expression tree using the Shunting Yard algorithm
def parseExpression(
    tokens: Vector[TokenInfo],
    opContext: OperatorsContext,
    reporter: Reporter[TyckError]
): Expr = {
  val output = mutable.Stack[TokenInfo]()
  val operators = mutable.Stack[TokenInfo]()

  tokens.zipWithIndex.foreach { case (tokenInfo, index) =>
    val prevToken = Option.when(index > 0)(tokens(index - 1))
    val nextToken =
      Option.when(index < tokens.length - 1)(tokens(index + 1))

    // Determine the operator type based on context
    val opType = determineOpType(tokenInfo, prevToken, nextToken)
    val tokenInfoWithType = tokenInfo.copy(possibleOpTypes = Set(opType))

    opType match {
      case OpType.Operand =>
        // Operands are pushed onto the output stack
        output.push(tokenInfoWithType)
      case OpType.Prefix | OpType.Postfix | OpType.Infix =>
        // Operators are pushed onto the operator stack after popping higher precedence operators
        while (
          operators.nonEmpty && shouldPopOperator(
            operators.top,
            tokenInfo.precedence,
            tokenInfo.associativity
          )
        )
          output.push(operators.pop())
        operators.push(tokenInfoWithType)
    }
  }

  // Pop any remaining operators onto the output stack
  while (operators.nonEmpty)
    output.push(operators.pop())

  // Build the expression tree from the output stack
  val expr = buildExpr(output, opContext, reporter)
  if (output.nonEmpty) {
    reporter.report(UnexpectedTokens(output.map(_.expr).toList))
  }
  expr
}

// Definition of operator types
sealed trait OpType extends Product with Serializable

object OpType {
  case object Infix extends OpType

  case object Prefix extends OpType

  case object Postfix extends OpType

  case object Operand extends OpType
}

// Holds information about each token in the expression
case class TokenInfo(
    expr: Expr,
    precedence: Int,
    associativity: Associativity,
    possibleOpTypes: Set[OpType],
    opInfos: Seq[(OpInfo, OpType)] = Seq.empty
)

// Resolves an operation sequence into an expression
def resolveOpSeq(
    reporter: Reporter[TyckError],
    opContext: OperatorsContext,
    opSeq: OpSeq
): Expr = {

  // Construct the precedence graph
  val precedenceGraph = constructPrecedenceGraph(opContext.groups)

  // Perform a topological sort of the precedence graph
  val topOrder = precedenceGraph.topologicalSort

  val groupPrecedence: Map[QualifiedIDString, Int] = topOrder.fold(
    { _ =>
      reporter.report(
        PrecedenceCycleDetected(precedenceGraph.nodes.map(_.outer))
      )
      Map.empty
    },
    { order =>
      val layeredOrder = order.toLayered
      // Map each group to its precedence level
      layeredOrder.iterator.flatMap { case (index, nodes) =>
        nodes.map(node => node.outer.name -> index)
      }.toMap
    }
  )

  // Parse tokens from the operation sequence
  val tokens = parseTokens(opSeq.seq, opContext, groupPrecedence, reporter)

  // Collect operator groups from tokens
  val operatorGroups = tokens.flatMap { tokenInfo =>
    tokenInfo.opInfos.collect { case (op: OpWithGroup, _) =>
      op.group
    }
  }

  // Verify that all operator groups are connected in the precedence graph
  for {
    group1 <- operatorGroups
    group2 <- operatorGroups
    if group1 != group2
    node1Option = precedenceGraph.find(group1)
    node2Option = precedenceGraph.find(group2)
    if node1Option.isDefined && node2Option.isDefined
    node1 = node1Option.get
    node2 = node2Option.get
    // Check if no path exists between group1 and group2
    if node1.pathTo(node2).isEmpty && node2.pathTo(node1).isEmpty
  }
    reporter.report(UnconnectedPrecedenceGroups(group1, group2))

  // Parse the expression from tokens
  parseExpression(tokens, opContext, reporter)
}
