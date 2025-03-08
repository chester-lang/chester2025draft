package chester.tyck

import chester.error._
import chester.syntax.concrete.Block
import chester.syntax.core.{FunctionType, Intersection, TelescopeTerm, ArgTerm, Union, LocalV, IntType, Type, LevelUnrestricted, StringType}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DependentTypeTest extends AnyFunSpec with Matchers {

  describe("Tyck with Dependent Types") {
    
    // Helper to parse and type check code
    def parseAndCheck(code: String): TyckResult = {
      val parsed = {
        val parseResult = chester.reader.Parser.parse(code)
        parseResult.get
      }
      
      // Create a real reporter to collect errors
      val reporter = new CollectingReporter[Problem]()
      
      // Type check the parsed program
      Tycker.check(parsed.asInstanceOf[Block])(using reporter)
    }
    
    // Helper to assert that two types are considered equivalent by the tyck system
    def assertTypesEquivalent(t1: String, t2: String): Unit = {
      val result = parseAndCheck(s"""
        |def testFn(x: $t1): $t2 = x;
        |testFn
      """.stripMargin)
      
      result match {
        case TyckResult1(_, _, errorReporter) => 
          errorReporter.problems should be(empty)
        case _ => fail("Type check failed or unexpected result type")
      }
    }
    
    it("should handle alpha-equivalent function types") {
      // These function types should be considered alpha-equivalent
      assertTypesEquivalent(
        "(x: Type) -> x", 
        "(y: Type) -> y"
      )
    }
    
    it("should handle alpha-equivalent nested function types") {
      // These nested function types should be considered alpha-equivalent
      assertTypesEquivalent(
        "(x: Type) -> (y: x) -> y", 
        "(a: Type) -> (b: a) -> b"
      )
    }
    
    it("should handle alpha-equivalent union types") {
      // These union types should be considered alpha-equivalent regardless of order
      assertTypesEquivalent(
        "Integer | String", 
        "String | Integer"
      )
    }
    
    it("should handle alpha-equivalent intersection types") {
      // These intersection types should be considered alpha-equivalent regardless of order
      assertTypesEquivalent(
        "Integer & String", 
        "String & Integer"
      )
    }
    
    it("should handle dependent record types") {
      val result = parseAndCheck("""
        |type Person = {
        |  name: String,
        |  age: Integer,
        |  greet: (other: {name: String}) -> String
        |};
        |
        |let person = {
        |  name: "Alice",
        |  age: 30,
        |  greet: (other) => "Hello, " + other.name
        |};
        |
        |// This should type check successfully
        |person.greet({name: "Bob"})
      """.stripMargin)
      
      result match {
        case TyckResult1(_, _, errorReporter) => 
          errorReporter.problems should be(empty)
        case _ => fail("Type check failed or unexpected result type")
      }
    }
    
    it("should correctly handle type-level computations") {
      val result = parseAndCheck("""
        |// Type-level function
        |def IdType(T: Type): Type = T;
        |
        |// Using the type-level function
        |let x: IdType(Integer) = 42;
        |
        |// This should be valid since IdType(Integer) reduces to Integer
        |let y: Integer = x;
      """.stripMargin)
      
      result match {
        case TyckResult1(_, _, errorReporter) => 
          errorReporter.problems should be(empty)
        case _ => fail("Type check failed or unexpected result type")
      }
    }
    
    it("should handle dependent pair types") {
      val result = parseAndCheck("""
        |// A dependent pair type (Σ-type)
        |type Pair(A: Type, B: A -> Type) = {
        |  first: A,
        |  second: B(first)
        |};
        |
        |// Creating a pair where the second element depends on the first
        |let intStringPair: Pair(Integer, (n) => String) = {
        |  first: 42,
        |  second: "42 as string"
        |};
        |
        |// Accessing the elements
        |let n: Integer = intStringPair.first;
        |let s: String = intStringPair.second;
      """.stripMargin)
      
      result match {
        case TyckResult1(_, _, errorReporter) => 
          errorReporter.problems should be(empty)
        case _ => fail("Type check failed or unexpected result type")
      }
    }
    
    // Direct test for the areAlphaEquivalent method
    it("should correctly identify alpha-equivalent terms programmatically") {
      // Create some test terms directly
      val functionType1 = FunctionType(
        Vector(TelescopeTerm(Vector(
          ArgTerm(LocalV("x", Type(LevelUnrestricted(None), None), None, None), Type(LevelUnrestricted(None), None), None, false, None)
        ), false, None)),
        LocalV("x", Type(LevelUnrestricted(None), None), None, None),
        None,
        None
      )
      
      val functionType2 = FunctionType(
        Vector(TelescopeTerm(Vector(
          ArgTerm(LocalV("y", Type(LevelUnrestricted(None), None), None, None), Type(LevelUnrestricted(None), None), None, false, None)
        ), false, None)),
        LocalV("y", Type(LevelUnrestricted(None), None), None, None),
        None,
        None
      )
      
      // Create a test context and state for tyck
      implicit val reporter: CollectingReporter[Problem] = new CollectingReporter[Problem]()
      implicit val tyck: Tyck = new Tyck()(using reporter)
      implicit val context: Context = Context.empty
      
      // Test if the types are considered alpha-equivalent
      val result = tyck.areAlphaEquivalent(functionType1, functionType2)
      result should be(true)
    }
  }
} 