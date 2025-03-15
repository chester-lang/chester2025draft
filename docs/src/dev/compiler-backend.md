# Chester Compiler Backend Architecture

## Overview

This document outlines the backend architecture of the Chester compiler system. The backend is responsible for transforming Chester's internal representation into executable code for various target platforms.

## Backend Pipeline

The Chester compiler backend follows a multi-phase code generation pipeline:

```
Core Representation → Target AST Generation → Optimization → Code Generation → Executable Artifacts
```

## Backend Components

The backend is composed of several key components:

### Target-Specific AST Generation

Each target platform has a specialized AST generation phase:

- **Target AST Construction**: Builds an AST specific to the target language
- **Type Mapping**: Maps Chester types to target language types
- **Effect Translation**: Converts Chester effects to target language constructs
- **Standard Library Binding**: Connects to platform-specific libraries

### Optimization Engine

The backend applies target-specific optimizations:

- **Dead Code Elimination**: Removes unused code
- **Constant Folding**: Evaluates constant expressions at compile time
- **Inlining**: Replaces function calls with function bodies where beneficial
- **Specialization**: Creates specialized versions of generic functions
- **Tail Call Optimization**: Optimizes tail-recursive calls

### Code Generation

The final phase transforms the optimized target AST to executable code:

- **Pretty Printing**: Generates formatted source code
- **Native Code Generation**: For targets that compile directly to machine code
- **Bytecode Generation**: For VM-based targets like the JVM
- **Source Maps**: Generates debugging information

## Supported Compiler Targets

Chester supports multiple compiler targets, each with its own backend implementation:

### JavaScript/TypeScript

The JavaScript/TypeScript target (`compiler/shared/src/main/scala/chester/targets/js/AST.scala`) enables running Chester programs in web browsers and Node.js.

#### Architecture

The JavaScript backend consists of:

1. **AST Nodes**: Comprehensive representation of JavaScript/TypeScript constructs
2. **Type Translator**: Maps Chester types to TypeScript type annotations
3. **Effect Handler**: Implements Chester's effect system using JavaScript promises
4. **Module System**: Generates ES module exports and imports

#### Key Features

- Complete JavaScript language support
- TypeScript type annotations
- ECMAScript module system
- Web API integration
- Sourcemap generation for debugging

#### JS AST Structure

The JavaScript AST is implemented as a set of case classes:

```scala
sealed trait ASTNode extends ToDoc {
  val meta: Option[Meta]
}

sealed trait Expression extends ASTNode
sealed trait Statement extends ASTNode
sealed trait Declaration extends Statement
sealed trait TypeAnnotation extends ASTNode
```

Each node includes a `toDoc` method for pretty printing and source generation.

### JVM (Java/Scala)

The JVM target enables integration with the Java ecosystem and leverages the JVM runtime.

#### Architecture

The JVM backend consists of:

1. **Bytecode Generation**: Direct generation of JVM bytecode
2. **Class Builder**: Creates JVM class files
3. **Runtime Library**: Core runtime support for Chester on JVM
4. **Java Interop**: Enables calling Java code from Chester

#### Key Features

- Java interoperability
- Scala library integration
- JVM optimizations
- Access to the Java standard library
- Advanced JVM optimizations (inlining, specialization)

### Native (Planned)

A native code target is planned for high-performance applications.

#### Potential Architecture

The native backend will likely include:

1. **LLVM IR Generation**: Translation to LLVM intermediate representation
2. **Native Runtime**: Minimal runtime support library 
3. **ABI Compatibility**: Interoperability with C/C++ code
4. **Platform Support**: Cross-compilation for different CPU architectures

#### Planned Features

- LLVM-based code generation
- Native performance
- Low-level memory control
- System programming capabilities
- Cross-platform support

## Type System Mapping

Chester's rich type system needs careful mapping to target language types:

| Chester Type        | JavaScript/TypeScript | JVM                | Native (Planned)    |
|---------------------|----------------------|--------------------|--------------------|
| Integer             | number               | scala.BigInt       | int64_t            |
| Natural             | number               | scala.BigInt       | uint64_t           |
| Boolean             | boolean              | scala.Boolean      | bool               |
| String              | string               | java.lang.String   | std::string        |
| Union Types (A|B)   | A \| B               | Specialized classes| Tagged unions      |
| Record              | interface/class      | case class         | struct             |
| Functions           | function             | Function objects   | Function pointers  |

## Effects Handling

Chester's effect system is implemented differently for each target language:

- **JavaScript/TypeScript**: Using promises or custom effect handlers
- **JVM**: Using exceptions and monadic structures
- **Native**: Using error codes or custom effect handling

## Implementation Example: JavaScript Backend

### JavaScript/TypeScript AST Example

The JavaScript target provides a good example of target-specific AST:

```scala
// Example: Function declaration in JavaScript AST
FunctionDeclaration(
  id = Some(Identifier("greet")),
  params = List(Parameter(TypedIdentifier("name", StringTypeAnnotation()))),
  returnType = Some(StringTypeAnnotation()),
  body = BlockStatement(List(
    ReturnStatement(Some(
      BinaryExpression(
        BinaryOperator.Plus,
        StringLiteral("Hello, "),
        Identifier("name")
      )
    ))
  ))
)
```

This represents the TypeScript function:

```typescript
function greet(name: string): string {
  return "Hello, " + name;
}
```

### JavaScript AST Node Categories

The JavaScript AST supports a wide range of node types:

#### Expressions

- **Literals**: Numbers, strings, booleans, null, BigInt, RegExp
- **Identifiers**: Named references (typed and untyped)
- **Operators**: Binary, logical, assignment, unary, update
- **Function Expressions**: Regular functions and arrow functions
- **Object and Array Expressions**: Object literals and array literals
- **Class Expressions**: Class definitions with inheritance and method definitions

#### Statements

- **Block Statements**: Groups of statements
- **Expression Statements**: Expressions used as statements
- **Control Flow Statements**: if/else, while, do-while, for, switch
- **Declaration Statements**: let, const, var declarations

#### TypeScript Features

- **Type Annotations**: For variables, parameters, return types
- **Interface and Type Declarations**: For defining complex types
- **Generics**: Type parameters for functions and classes
- **Union and Intersection Types**: Type combinations

## Build System Integration

The Chester compiler backend integrates with build systems through:

- **SBT Plugin**: For JVM builds
- **NPM Package**: For JavaScript/TypeScript integration
- **CLI Interface**: For command-line usage

## Future Directions

Planned improvements to the compiler backend include:

- **WebAssembly Support**: Direct compilation to WebAssembly
- **More Native Targets**: Support for various native platforms
- **Interoperability Enhancements**: Better interop with target languages
- **Performance Optimizations**: Target-specific optimizations
- **Cross-Compilation**: Single-command compilation to multiple targets
- **Advanced Optimizations**: Target-specific performance improvements

## References

- JavaScript AST is inspired by the [ESTree Spec](https://github.com/estree/estree)
- JVM codegen draws from [Scala 3 compiler techniques](https://github.com/lampepfl/dotty)
- LLVM-based compilation follows the [LLVM Language Reference](https://llvm.org/docs/LangRef.html) 