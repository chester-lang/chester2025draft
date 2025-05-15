package chester.tyck

import chester.error.{TyckProblem, VectorReporter}
import chester.elab.{DefaultElaborator, ElabOps}
import chester.reader.FileNameAndContent
import chester.readerv2.ChesterReaderV2
import chester.syntax.core.*
import chester.tyck.api.NoopSemanticCollector
import munit.FunSuite

class ElabLiteralAndListTest extends FunSuite {
  
  test("integer literal typechecking with new elab") {
    // Parse the integer literal expression using FileNameAndContent
    val expr = ChesterReaderV2.parseExpr(FileNameAndContent("literal.chester", "1")).fold(
      error => fail(s"Failed to parse expression: $error"),
      identity
    )
    
    // Create reporter and ElabOps for typechecking
    val reporter = new VectorReporter[TyckProblem]()
    val elabOps = ElabOps(reporter, NoopSemanticCollector)
    
    // Infer the type using the DefaultElaborator
    val judge = DefaultElaborator.inferPure(expr)(using elabOps)
    
    // Assert that there are no errors
    assertEquals(reporter.getReports.isEmpty, true, 
      s"Expected no type errors, but got: ${reporter.getReports}")
    
    // Check that the elaborated term is an IntTerm
    // The new elaboration system correctly types small integers as IntTerm
    assert(judge.wellTyped.isInstanceOf[IntTerm], 
      s"Expected IntTerm but got ${judge.wellTyped.getClass.getSimpleName}")
      
    // Check that the type is IntType
    assert(judge.ty.isInstanceOf[IntType], 
      s"Expected IntType but got ${judge.ty.getClass.getSimpleName}")
  }
  
  test("heterogeneous list typechecking with new elab") {
    // Parse the list expression with mixed types
    val expr = ChesterReaderV2.parseExpr(FileNameAndContent("list.chester", "[1, \"a\"]")).fold(
      error => fail(s"Failed to parse expression: $error"),
      identity
    )
    
    // Create reporter and ElabOps for typechecking
    val reporter = new VectorReporter[TyckProblem]()
    val elabOps = ElabOps(reporter, NoopSemanticCollector)
    
    // Infer the type using the DefaultElaborator
    val judge = DefaultElaborator.inferPure(expr)(using elabOps)
    
    // Assert that there are no errors
    assertEquals(reporter.getReports.isEmpty, true, 
      s"Expected no type errors, but got: ${reporter.getReports}")
    
    // Check that the elaborated term is a ListTerm
    assert(judge.wellTyped.isInstanceOf[ListTerm], 
      s"Expected ListTerm but got ${judge.wellTyped.getClass.getSimpleName}")
      
    // Get the list terms and verify their types
    val listTerm = judge.wellTyped.asInstanceOf[ListTerm]
    assertEquals(listTerm.terms.size, 2, "List should have 2 elements")
    
    // First element should be an int term (not IntegerTerm - the elaborator chooses more specific types)
    assert(listTerm.terms(0).isInstanceOf[IntTerm], 
      s"First element should be IntTerm but got ${listTerm.terms(0).getClass.getSimpleName}")
    
    // Second element should be a string
    assert(listTerm.terms(1).isInstanceOf[StringTerm], 
      s"Second element should be StringTerm but got ${listTerm.terms(1).getClass.getSimpleName}")

    // Check the list type structure
    assert(judge.ty.isInstanceOf[ListType], 
      s"Expected ListType but got ${judge.ty.getClass.getSimpleName}")
    
    val listType = judge.ty.asInstanceOf[ListType]
    
    // Verify that the element type is a Union type
    assert(listType.ty.isInstanceOf[Union], 
      s"Expected Union type for list elements but got ${listType.ty.getClass.getSimpleName}")
    
    // Verify the union contains both Int and String types (order may vary)
    val unionTypes = listType.ty.asInstanceOf[Union].xs
    
    // Check that the union has exactly 2 types
    assertEquals(unionTypes.length, 2, s"Expected 2 types in the union but got ${unionTypes.length}")
    
    // Check that the union contains both IntType and StringType, regardless of order
    val hasIntType = unionTypes.exists(_.isInstanceOf[IntType])
    val hasStringType = unionTypes.exists(_.isInstanceOf[StringType])
    
    assert(hasIntType, "Union type should contain IntType")
    assert(hasStringType, "Union type should contain StringType")
  }
  
  test("empty list typechecking with new elab") {
    // Parse the empty list expression
    val expr = ChesterReaderV2.parseExpr(FileNameAndContent("empty-list.chester", "[]")).fold(
      error => fail(s"Failed to parse expression: $error"),
      identity
    )
    
    // Create reporter and ElabOps for typechecking
    val reporter = new VectorReporter[TyckProblem]()
    val elabOps = ElabOps(reporter, NoopSemanticCollector)
    
    // Infer the type using the DefaultElaborator
    val judge = DefaultElaborator.inferPure(expr)(using elabOps)
    
    // Assert that there are no errors
    assertEquals(reporter.getReports.isEmpty, true, 
      s"Expected no type errors, but got: ${reporter.getReports}")
    
    // Check that the elaborated term is a ListTerm
    assert(judge.wellTyped.isInstanceOf[ListTerm], 
      s"Expected ListTerm but got ${judge.wellTyped.getClass.getSimpleName}")
      
    // Get the list terms and verify it's empty
    val listTerm = judge.wellTyped.asInstanceOf[ListTerm]
    assertEquals(listTerm.terms.size, 0, "List should have 0 elements")

    // Check the list type structure
    assert(judge.ty.isInstanceOf[ListType], 
      s"Expected ListType but got ${judge.ty.getClass.getSimpleName}")
    
    val listType = judge.ty.asInstanceOf[ListType]
    
    // Verify that the element type is NothingType
    assert(listType.ty.isInstanceOf[NothingType], 
      s"Expected NothingType for empty list element type but got ${listType.ty.getClass.getSimpleName}")
  }
  
  test("nested list typechecking with new elab") {
    // Parse the nested list expression [[1], [2]]
    val expr = ChesterReaderV2.parseExpr(FileNameAndContent("nested-list.chester", "[[1], [2]]")).fold(
      error => fail(s"Failed to parse expression: $error"),
      identity
    )
    
    // Create reporter and ElabOps for typechecking
    val reporter = new VectorReporter[TyckProblem]()
    val elabOps = ElabOps(reporter, NoopSemanticCollector)
    
    // Infer the type using the DefaultElaborator
    val judge = DefaultElaborator.inferPure(expr)(using elabOps)
    
    // Assert that there are no errors
    assertEquals(reporter.getReports.isEmpty, true, 
      s"Expected no type errors, but got: ${reporter.getReports}")
    
    // Check that the elaborated term is a ListTerm
    assert(judge.wellTyped.isInstanceOf[ListTerm], 
      s"Expected ListTerm but got ${judge.wellTyped.getClass.getSimpleName}")
    
    // Get the list terms and verify they are also ListTerms
    val outerListTerm = judge.wellTyped.asInstanceOf[ListTerm]
    assertEquals(outerListTerm.terms.size, 2, "Outer list should have 2 elements")
    
    // First element should be a ListTerm
    assert(outerListTerm.terms(0).isInstanceOf[ListTerm], 
      s"First element should be ListTerm but got ${outerListTerm.terms(0).getClass.getSimpleName}")
    
    // Second element should be a ListTerm
    assert(outerListTerm.terms(1).isInstanceOf[ListTerm], 
      s"Second element should be ListTerm but got ${outerListTerm.terms(1).getClass.getSimpleName}")
    
    // Verify the inner lists contain IntTerm elements
    val firstInnerList = outerListTerm.terms(0).asInstanceOf[ListTerm]
    val secondInnerList = outerListTerm.terms(1).asInstanceOf[ListTerm]
    
    assert(firstInnerList.terms(0).isInstanceOf[IntTerm], 
      s"Inner list element should be IntTerm but got ${firstInnerList.terms(0).getClass.getSimpleName}")
    
    assert(secondInnerList.terms(0).isInstanceOf[IntTerm], 
      s"Inner list element should be IntTerm but got ${secondInnerList.terms(0).getClass.getSimpleName}")

    // Check the list type structure for List(List(Int))
    assert(judge.ty.isInstanceOf[ListType], 
      s"Expected outer ListType but got ${judge.ty.getClass.getSimpleName}")
    
    val outerListType = judge.ty.asInstanceOf[ListType]
    
    // Verify that the element type is a ListType
    assert(outerListType.ty.isInstanceOf[ListType], 
      s"Expected ListType but got ${outerListType.ty.getClass.getSimpleName}")
    
    // Verify that the inner list element type is IntType
    val innerListType = outerListType.ty.asInstanceOf[ListType]
    assert(innerListType.ty.isInstanceOf[IntType], 
      s"Expected IntType for innermost element but got ${innerListType.ty.getClass.getSimpleName}")
  }
}
