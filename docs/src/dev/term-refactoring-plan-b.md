# Term Representation Refactoring: Plan B

## Current Situation

Currently, the Chester compiler has multiple Term representations:

1. **Term.scala** - Abstract interfaces and traits that define the structure of terms
2. **truffle.scala** - JVM-specific term implementation built on GraalVM Truffle
3. **simple.scala** - A simpler implementation used in JS/Native platforms

This structure introduces complexity with various type classes and traits with `*C` and `*T` suffixes for cross-platform compatibility.

## Alternative Approach: Single Term Representation

Instead of maintaining two parallel implementations, this plan proposes:

1. **Remove `simple.scala`**: Eliminate the separate JS/Native implementation
2. **Make `truffle.scala` shared across all platforms**: Use a single term representation for JVM, JS, and Native
3. **Remove `Term.scala` abstractions**: Eliminate the abstract interfaces
4. **Define no-op `@child` and `@const` annotations for JS/Native**: Maintain source compatibility while eliminating runtime overhead

## Refactoring Goals

We aim to simplify the codebase by:

1. **Unify term representation**: Use a single term implementation across all platforms
2. **Remove `*C` and `*T` type abstractions**: Simplify the codebase by removing these cross-platform compatibility abstractions
3. **Reduce code duplication**: Maintain a single source of truth for term definition and serialization
4. **Maintain platform-specific optimizations**: Use annotations to enable JVM-specific Truffle optimizations

## Implementation Plan

### Phase 1: Code Analysis

1. Identify all uses of `Term.scala` and `simple.scala` across the codebase
2. Map the dependencies on platform-specific features in `truffle.scala`
3. Identify all platform-specific annotations used in `truffle.scala`

### Phase 2: Create Cross-Platform Annotations

1. Define no-op versions of `@child` and `@const` annotations for JS/Native platforms:
   ```scala
   // For JVM
   // (Use existing Truffle annotations)
   
   // For JS/Native
   package chester.syntax.core.truffle
   
   import scala.annotation.StaticAnnotation
   
   class child extends StaticAnnotation
   class const extends StaticAnnotation
   ```

2. Create platform-specific build configurations to include the appropriate annotation implementation
3. Verify the annotations work in JVM mode with Truffle and as no-ops in JS/Native

### Phase 3: Migrate to Single Term Implementation

1. Update `truffle.scala` to work on all platforms by:
   - Identifying and isolating platform-specific functionality
   - Ensuring all dependencies work across platforms
   - Adding conditional compilation for platform-specific code

2. Update all code that currently depends on `simple.scala` to use `truffle.scala` instead
3. Gradually remove `Term.scala` abstractions

### Phase 4: Serialization Implementation

1. Implement manual `ReadWriter` instances for terms:
   ```scala
   implicit val rw: ReadWriter[Term] = readwriter[IntermediateType].bimap[Term](
     fromTerm => intermediateValue,
     intermediateValue => new Term(...)
   )
   ```

2. Create a serialization protocol that works across all platforms
3. Test serialization/deserialization across platform boundaries

### Phase 5: Testing & Cleanup

1. Ensure all tests pass on JVM, JS, and Native platforms
2. Remove `simple.scala` and `Term.scala`
3. Update documentation to reflect the new unified structure

## Migration Considerations

### Platform-Specific Features

The JVM platform uses GraalVM Truffle for optimizations:
- `@child` and `@const` annotations enable Truffle optimizations on JVM
- On JS/Native platforms, these will be no-op annotations
- Code will be identical across platforms, but behavior will be optimized on JVM

### Performance Impact

This approach offers several benefits:
- Unified codebase with less maintenance overhead
- Single implementation reduces bugs from synchronization issues
- JVM platform maintains Truffle optimizations
- JS/Native platforms avoid unnecessary abstraction layers
- Single serialization format works across all platforms

### Potential Challenges

1. **GraalVM Truffle Dependencies**: Ensuring Truffle dependencies don't cause issues on JS/Native
2. **Platform-Specific Code**: Identify and handle any platform-specific behaviors
3. **Performance on JS/Native**: Monitor performance to ensure the unified implementation doesn't negatively impact JS/Native platforms

## Specific Terms to Refactor

The refactoring will affect the same terms as Plan A, but the approach differs by unifying implementation rather than maintaining separate versions:

1. **Term Core Representations**:
   - `Term` - Unified across platforms
   - `WHNF`, `Uneval`, `SpecialTerm`, etc. - Single implementations

2. **Class/Record/Trait Definitions**:
   - `ObjectStmtTerm`, `TraitStmtTerm`, etc. - Single implementations with platform-specific annotations

3. **Call Terms**:
   - `ObjectCallTerm`, `TraitCallTerm`, etc. - Single implementations with platform-specific optimizations

4. **Type Terms and Literals**:
   - All term types will have unified implementations
   - Platform-specific behaviors controlled via annotations 