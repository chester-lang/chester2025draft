# Testing Guide

## Running Tests

### Module-Specific Tests

Always run tests for specific modules to get faster feedback:

```bash
# Run tests for specific module
sbt "semantic/test"
sbt "reader/test"

# Run specific test class
sbt "semantic/testOnly chester.tyck.LazyReductionTest"
```

### Platform-Specific Tests

```bash
# Run all JVM tests
sbt "rootJVM/test"

# Run all JS tests
sbt "rootJS/test"
```

### Why This Matters

- Faster feedback loop
- More focused debugging
- Prevents unnecessary test runs
- Saves CI/CD resources
- Avoids cross-platform compilation overhead

## Writing Tests

### Test Organization

1. **Group Tests by Functionality**
   ```scala
   class TypeCheckerTest extends FunSuite {
     // Related tests together
     test("record field access - direct") { ... }
     test("record field access - lazy") { ... }
     test("record field access - error cases") { ... }
   }
   ```

2. **Test Both Success and Error Cases**
   ```scala
   test("type checking errors") {
     val invalidExpr = // ...
     val result = tycker.check(invalidExpr)
     result match {
       case TyckResult.Failure(errors, _, _, _) =>
         assert(errors.exists(_.isInstanceOf[TypeError]))
       case _ => fail("Expected type error")
     }
   }
   ```

### Cross-Platform Testing

1. **Platform-Agnostic Imports**
   ```scala
   // CORRECT
   import chester.syntax.core.*
   import chester.error.*
   
   // INCORRECT - Platform specific
   import chester.syntax.core.truffle.*
   ```

2. **Platform-Independent Assertions**
   ```scala
   // CORRECT
   test("record type") {
     val record = Record("x" -> IntType)
     assertEquals(record.fieldType("x"), IntType)
   }
   
   // INCORRECT - Platform specific
   test("JVM record") {
     val record = JVMRecord(...)
     assertTrue(record.isInstanceOf[JVMRecordImpl])
   }
   ```

3. **Test Helper Methods**
   ```scala
   trait TestHelpers {
     // Create test data consistently
     def createTestRecord(name: String, ty: Type): Record = ...
     def createTestFunction(params: List[Type], ret: Type): Function = ...
   }
   ```

## Common Issues

### Test Performance

1. **Avoid Unnecessary Reduction**
   ```scala
   // CORRECT - Only reduce when needed
   val result = expr match {
     case record: RecordTerm => record
     case _ => reducer.reduce(expr)
   }
   
   // INCORRECT - Always reducing
   val result = reducer.reduce(expr)
   ```

2. **Cache Test Data**
   ```scala
   class TypeCheckerTest extends FunSuite {
     // Reuse common test data
     private val commonTypes = Map(
       "Int" -> IntType,
       "Bool" -> BoolType
     )
   }
   ```

### Error Handling

1. **Clear Error Messages**
   ```scala
   // CORRECT
   fail(s"Type checking failed: ${errors.mkString(", ")}")
   
   // INCORRECT
   fail("Test failed")
   ```

2. **Test Error Cases**
   ```scala
   test("invalid field access") {
     val record = Record("x" -> IntType)
     val result = tycker.check(DotCall(record, "y"))
     assertMatch(result) {
       case TyckResult.Failure(errors, _, _, _) =>
         assert(errors.exists(_.message.contains("unknown field")))
     }
   }
   ```

## Test Documentation

1. **Test Names**
   - Clear and descriptive
   - Indicate what's being tested
   - Show expected behavior

2. **Test Comments**
   - Explain complex test setup
   - Document test data generation
   - Note platform-specific behavior

3. **Example**
   ```scala
   /** Tests record field access with lazy reduction.
    * 
    * This test verifies that:
    * 1. Direct record access works without reduction
    * 2. Function calls are reduced only when needed
    * 3. Type-level computation preserves original terms
    */
   test("field access - lazy reduction") {
     // Test implementation
   }
   ```
