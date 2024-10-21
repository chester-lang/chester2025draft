package chester.tyck

import chester.syntax.concrete._
import chester.syntax.core._

trait MethodExtractor {

  // Extract methods from a concrete syntax (Expr)
  def extractMethodsFromExpr(expr: Expr): Seq[LetDefStmt] = {
    expr match {
      case record: RecordStmt       => collectMethodsFromBody(record.body)
      case traitStmt: TraitStmt     => collectMethodsFromBody(traitStmt.body)
      case interfaceStmt: InterfaceStmt => collectMethodsFromBody(interfaceStmt.body)
      case objectStmt: ObjectStmt   => collectMethodsFromBody(objectStmt.body)
      case _ => Seq.empty
    }
  }

  // Extract methods from a core syntax (Term)
  def extractMethodsFromTerm(term: Term): Seq[DefStmtTerm] = {
    term match {
      case record: RecordStmtTerm       => collectMethodsFromBodyTerm(record.body)
      case traitStmt: TraitStmtTerm     => collectMethodsFromBodyTerm(traitStmt.body)
      case interfaceStmt: InterfaceStmtTerm => collectMethodsFromBodyTerm(interfaceStmt.body)
      case objectStmt: ObjectStmtTerm   => collectMethodsFromBodyTerm(objectStmt.body)
      case _ => Seq.empty
    }
  }

  // Helper method to collect methods from Option[Block]
  private def collectMethodsFromBody(body: Option[Block]): Seq[LetDefStmt] = {
    body.map(collectMethodsFromBlock).getOrElse(Seq.empty)
  }

  // Helper method to collect methods from Block
  private def collectMethodsFromBlock(block: Block): Seq[LetDefStmt] = {
    block.heads.collect {
      case letDefStmt: LetDefStmt if letDefStmt.kind == LetDefType.Def => letDefStmt
    } ++ block.tail.toSeq.flatMap(collectMethodsFromExpr)
  }

  // Overloaded method for Term
  private def collectMethodsFromBodyTerm(body: Option[BlockTerm]): Seq[DefStmtTerm] = {
    body.map(collectMethodsFromBlockTerm).getOrElse(Seq.empty)
  }

  private def collectMethodsFromBlockTerm(block: BlockTerm): Seq[DefStmtTerm] = {
    block.statements.collect {
      case defStmtTerm: DefStmtTerm => defStmtTerm
    } ++ collectMethodsFromTerm(block.result)
  }

  // Recursive method to collect methods from an Expr
  private def collectMethodsFromExpr(expr: Expr): Seq[LetDefStmt] = {
    expr match {
      case block: Block => collectMethodsFromBlock(block)
      case _            => Seq.empty
    }
  }

  // Recursive method to collect methods from a Term
  private def collectMethodsFromTerm(term: Term): Seq[DefStmtTerm] = {
    term match {
      case block: BlockTerm => collectMethodsFromBlockTerm(block)
      case _                => Seq.empty
    }
  }

}