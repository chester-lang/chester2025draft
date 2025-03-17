# Term Representation Refactoring Plan

## Current Situation

Currently, the Chester compiler has multiple Term representations:

1. **Term.scala** - Abstract interfaces and traits that define the structure of terms
2. **truffle.scala** - JVM-specific term implementation built on GraalVM Truffle
3. **simple.scala** - A simpler implementation used in JS/Native platforms

This structure introduces complexity with various type classes and traits with `*C` and `*T` suffixes for cross-platform compatibility.

## Refactoring Scope

The following table outlines the number of code elements that will need to be refactored:

| Element Type       | Term.scala | truffle.scala | simple.scala | Total |
|--------------------|------------|---------------|--------------|-------|
| Traits             | TBD        | TBD           | TBD          | TBD   |
| Case Classes       | TBD        | TBD           | TBD          | TBD   |
| Regular Classes    | TBD        | TBD           | TBD          | TBD   |
| *C Suffix Traits   | TBD        | TBD           | TBD          | TBD   |
| *T Suffix Traits   | TBD        | TBD           | TBD          | TBD   |
| *F Suffix Classes  | TBD        | TBD           | TBD          | TBD   |
| Total Elements     | TBD        | TBD           | TBD          | TBD   |

Note: The exact counts will be determined during the Code Analysis phase.

### Specific Terms to Refactor

The refactoring will affect many specific term implementations, including (but not limited to):

1. **Term Core Representations**:
   - `Term` - The base interface
   - `WHNF` - Weak Head Normal Form
   - `Uneval` - Unevaluated expressions
   - `SpecialTerm` - Special term types
   - `TermWithUniqid` - Terms with unique identifiers

2. **Class/Record/Trait Definitions**:
   - `ObjectStmtTerm` - Object statement terms
   - `TraitStmtTerm` - Trait statement terms
   - `RecordStmtTerm` - Record statement terms
   - `InterfaceStmtTerm` - Interface statement terms
   - `TypeDefinition` - Type definition terms

3. **Call Terms**:
   - `ObjectCallTerm` - Object call terms
   - `TraitCallTerm` - Trait call terms
   - `RecordCallTerm` - Record call terms
   - `RecordConstructorCallTerm` - Record constructor call terms

4. **Type Terms**:
   - `TypeTerm` - Type terms
   - `FunctionType` - Function type terms
   - `ObjectTypeTerm` - Object type terms

5. **Literals and Basic Terms**:
   - `IntTerm`, `IntegerTerm`, `StringTerm` - Literal terms
   - `BlockTerm` - Block terms
   - `FieldTerm` - Field terms
   - `LocalV`, `ToplevelV` - Variable references

## Refactoring Goals

We aim to simplify the codebase by:

1. **Remove `Term.scala`**: Eliminate the abstract interfaces and directly use platform-specific implementations
2. **Make `truffle.scala` the only Term for JVM**: Use the Truffle-based implementation exclusively for the JVM platform
3. **Make `simple.scala` the only Term for JS/Native**: Use the simpler implementation for JS and Native platforms
4. **Remove `*C` and `*T` type abstractions**: Simplify the codebase by removing these cross-platform compatibility abstractions

## Implementation Plan

### Phase 1: Code Analysis

1. Identify all uses of `Term.scala` across the codebase
2. Map the relationships between `Term.scala`, `truffle.scala`, and `simple.scala`
3. Identify all places where `*C` and `*T` suffixed traits are used

### Phase 2: Restructuring

1. Update `truffle.scala` to be self-contained without depending on `Term.scala`
2. Update `simple.scala` to be self-contained without depending on `Term.scala`
3. Create any necessary conversion utilities between the two implementations
4. Update build configurations to use the appropriate Term implementation per platform

### Phase 3: Serialization Implementation

1. Implement manual `ReadWriter` instances for non-case classes in `truffle.scala`
   - Unlike case classes which can use `derives ReadWriter` in Scala 3, regular classes need manual implementation
   - Use the `bimap` approach for custom serialization:
   ```scala
   implicit val rw: ReadWriter[MyClass] = readwriter[IntermediateType].bimap[MyClass](
     fromClass => intermediateValue,
     intermediateValue => new MyClass(...)
   )
   ```
   - Create a shared serialization protocol for both platforms to ensure data compatibility

2. Ensure `simple.scala` maintains the `derives ReadWriter` for its case classes
   - This works automatically as it uses case classes exclusively

3. Implement conversion utilities between the two representations for cross-platform compatibility

### Phase 4: Dependency Updates

1. Update all code that currently depends on `Term.scala` to use the platform-appropriate implementation
2. Remove uses of `*C` and `*T` suffix patterns throughout the codebase

### Phase 5: Testing & Cleanup

1. Ensure all tests pass on both JVM and JS/Native platforms
2. Remove `Term.scala` and any unused code
3. Update documentation to reflect the new structure

## Migration Considerations

### Serialization

The current serialization system uses ReadWriter instances that may need to be updated:

- JVM platform: Will use manually implemented `ReadWriter` instances for `truffle.scala` classes
- JS/Native platform: Will use automatically derived `ReadWriter` instances via `derives ReadWriter` syntax

A shared serialization protocol is essential to ensure data can be exchanged between platforms:
- Use a stable serialization schema that both platforms agree on
- Implement conversion utilities if needed for cross-platform data exchange
- Consider using intermediate representations for complex objects

### Cross-Platform Code

Code that needs to run on both platforms will need special consideration:
- Shared code may need to use type classes or interfaces
- Platform-specific implementations will be provided via the build system

### Performance Impact

The removal of abstraction layers should improve performance:
- Fewer indirections and type conversions
- More direct use of platform capabilities (especially Truffle on JVM) 