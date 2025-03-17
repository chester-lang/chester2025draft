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

## Direct Migration Plan

This outlines a direct approach to implementing Plan B, immediately removing `simple.scala` and `Term.scala` while making `truffle.scala` work across all platforms.

### Phase 1: Prepare truffle.scala for All Platforms

1. ✅ Create no-op versions of `@child`, `@const`, and `@children` annotations for JS platform
2. ✅ Create no-op versions of `@child`, `@const`, and `@children` annotations for Native platform
3. Create abstraction for JVM-specific dependencies in truffle.scala:
   - Abstract over VirtualFrame for non-JVM platforms
   - Handle Node inheritance for non-JVM platforms
   - Conditionally include executeGeneric method

### Phase 2: Fix Serialization 

1. Keep existing serialization in truffle.scala, but make it work without simple.scala
2. Use derives ReadWriter where possible for case classes
3. Implement manual ReadWriter instances for non-case classes

### Phase 3: Remove Dependencies and Update Build Config

1. Remove all imports of:
   - `chester.syntax.core.simple.*`
   - `chester.syntax.core.spec.spec.*` (from Term.scala)
   
2. Update all code to use `chester.syntax.core.truffle.*` instead

3. Update build.sbt to:
   - Remove simple.scala from builds for JS/Native 
   - Use platform-specific no-op annotations
   - Make truffle.scala available to all platforms

### Phase 4: Delete Unused Files

1. Delete `simple.scala` after confirming all references are updated
2. Delete `Term.scala` after confirming all references are updated

## Implementation Order

1. First implement the JVM-specific abstraction
2. Update the serialization code
3. Fix imports for key modules one at a time
4. Run tests after each major change
5. Finally remove unused files after all tests pass

## Migration Status Tracking

| Task | Status | Notes |
|------|--------|-------|
| No-op annotations for JS | ✅ Complete | |
| No-op annotations for Native | ✅ Complete | |
| JVM-specific abstraction | Not started | |
| Update serialization | Not started | |
| Update imports | Not started | |
| Remove unused files | Not started | |

## Platform-Specific Features

The JVM platform uses GraalVM Truffle for optimizations:
- `@child` and `@const` annotations enable Truffle optimizations on JVM
- On JS/Native platforms, these will be no-op annotations
- Code will be identical across platforms, but behavior will be optimized on JVM

## Performance Impact

This approach offers several benefits:
- Unified codebase with less maintenance overhead
- Single implementation reduces bugs from synchronization issues
- JVM platform maintains Truffle optimizations
- JS/Native platforms avoid unnecessary abstraction layers
- Single serialization format works across all platforms

## Potential Challenges

1. **GraalVM Truffle Dependencies**: Ensuring Truffle dependencies don't cause issues on JS/Native
2. **Platform-Specific Code**: Identify and handle any platform-specific behaviors
3. **Performance on JS/Native**: Monitor performance to ensure the unified implementation doesn't negatively impact JS/Native platforms

## Specific Terms to Refactor

The refactoring will affect many specific term implementations, including:

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