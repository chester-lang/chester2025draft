# Term Refactoring: Migration Steps

This document outlines the detailed steps for implementing Plan B - unifying term representation around `truffle.scala`.

## Phase 1: Groundwork

1. ✅ Create no-op versions of `@child`, `@const`, and `@children` annotations for JS platform
2. ✅ Create no-op versions of `@child`, `@const`, and `@children` annotations for Native platform

## Phase 2: Move truffle.scala to Shared

1. Create a new file in the shared directory:
   ```
   syntax/shared/src/main/scala/chester/syntax/core/term.scala
   ```

2. Copy the entire contents of `truffle.scala` to `term.scala`, excluding JVM-specific imports:
   - Remove `import com.oracle.truffle.api.frame.VirtualFrame`
   - Remove `extends com.oracle.truffle.api.nodes.Node` from the `Term` class
   - Remove `executeGeneric` method or conditionally compile it
   - Leave `@child`, `@const`, and `@children` annotations (they'll be platform-specific)

3. Create platform-specific imports for term.scala:
   - Add conditional imports for VirtualFrame on JVM
   - Use no-op versions of annotations on JS/Native

## Phase 3: Update Build Configuration

1. Update build.sbt to:
   - Include shared `term.scala` in the build for all platforms
   - Exclude `simple.scala` from JS/Native builds
   - Include platform-specific `truffleutils.scala` files

## Phase 4: Refactor Serialization

1. Create standalone serialization utilities:
   ```
   syntax/shared/src/main/scala/chester/syntax/core/serialization.scala
   ```

2. Implement manual `ReadWriter` instances for all term types:
   ```scala
   given rw: ReadWriter[Term] = readwriter[IntermediateType].bimap(fromTerm, toTerm)
   // Additional instances for all term types
   ```

3. Remove serialization code from `truffle.scala` and `simple.scala`

## Phase 5: Update References

1. Update imports in all files that reference:
   - `chester.syntax.core.simple.*`
   - `chester.syntax.core.truffle.*`
   - `chester.syntax.core.spec.spec.*`

2. Replace them with:
   - `chester.syntax.core.term.*`

## Phase 6: Clean Up

1. Remove `Term.scala` once all references have been migrated
2. Remove `simple.scala` once all references have been migrated
3. Remove old `truffle.scala` after confirming the shared version works

## Implementation Order

To reduce risk and ensure we can roll back if needed, we should proceed in this order:

1. Complete Phase 1 and Phase 2 first
2. Create temporary dual-import support to allow gradual migration
3. Update one module at a time, starting with smaller, less critical modules
4. Run tests after each module update
5. Complete full clean-up only after all tests pass

## Migration Status Tracking

| Module | Status | Issues |
|--------|--------|--------|
| syntax/js | Phase 1 ✅ | None |
| syntax/native | Phase 1 ✅ | None |
| syntax/jvm | Not started | |
| syntax/shared | Not started | |

## Special Considerations

### JVM-Specific Functionality

The `executeGeneric` method and Truffle Node inheritance are JVM-specific:

```scala
// JVM-specific
final def executeGeneric(frame: VirtualFrame): Object = 
  globalExecuteGeneric.get(frame, this)
```

We'll need to conditionally compile this functionality using:

```scala
// Shared code
sealed abstract class Term extends TermT[Term] {
  type ThisTree <: Term
  
  // JVM-specific functionality
  #ifdef JVM
  extends com.oracle.truffle.api.nodes.Node {
    final def executeGeneric(frame: VirtualFrame): Object = 
      globalExecuteGeneric.get(frame, this)
  }
  #endif
} 