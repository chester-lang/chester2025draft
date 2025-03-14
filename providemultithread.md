# ProvideMultithread Implementation Guide

This document outlines the necessary changes to properly implement the multithreaded version of the propagator system.

## Core Issues to Fix

The current `ProvideMultithread.scala` implementation has several issues that prevent it from working correctly:

1. **Eagerness in using default values** - The implementation tries to use default values too eagerly
2. **Lack of proper change tracking** - Changes to cells aren't properly tracked and propagated
3. **Missing recursion depth control** - No mechanism to prevent stack overflow in recursive operations
4. **No cycle detection** - Can enter infinite loops when processing interdependent cells
5. **Thread safety issues** - Not properly using atomic operations and concurrent collections

## Implementation Requirements

### 1. HoldCell Enhancements

```scala
class HoldCell[+T <: Cell[?]](uniqId: UniqidOf[Impl[?]], initialValue: T) {
  // Add change tracking flag
  private val didChangeRef = new AtomicBoolean(false)
  
  // Add methods to check and clear change status
  def didChange: Boolean = didChangeRef.get()
  def clearDidChange(): Boolean = didChangeRef.getAndSet(false)
  
  // Update compareAndSetStore to track changes
  def compareAndSetStore(expectedValue: Cell[?], newValue: Cell[?]): Boolean = {
    val result = storeRef.compareAndSet(expectedValue, newValue)
    if (result) {
      didChangeRef.set(true)
    }
    result
  }
}
```

### 2. Impl Class Enhancements

```scala
class Impl[Ability](...) extends StateAbility[Ability] {
  // Use ConcurrentHashMap for cell and propagator storage
  private val cells = new ConcurrentHashMap[String, HoldCell[Cell[?]]]()
  private val propagators = new ConcurrentHashMap[String, HoldPropagator[Propagator[?]]]()
  
  // Add thread-safe change tracking queue
  private val didChanged = new ConcurrentLinkedQueue[CIdOf[?]]()
  
  // Add progress tracking flag
  private val didSomethingRef = new AtomicBoolean(false)
  def didSomething: Boolean = didSomethingRef.get()
  def setDidSomething(value: Boolean): Unit = didSomethingRef.set(value)
  
  // Add recursion depth tracking using ThreadLocal
  private val recursionDepthThreadLocal = new ThreadLocal[Integer] {
    override def initialValue(): Integer = 0
  }
  
  // Methods for recursion depth management
  private def getRecursionDepth(): Int = /* ... */
  private def setRecursionDepth(depth: Int): Unit = /* ... */
  private def incrementRecursionDepth(): Int = /* ... */
  private def decrementRecursionDepth(): Unit = /* ... */
}
```

### 3. Update Method Enhancement

```scala
override def update[T <: Cell[U], U](id: CIdOf[T], f: T => T)(using Ability): Unit = {
  // Existing implementation...
  if (updated) {
    // Track that something changed
    setDidSomething(true)
    // Add to the changed queue
    didChanged.add(id)
    // Process propagators
    processPropagators(id)
  }
}
```

### 4. Tick Method Implementation

```scala
override def tick(using Ability): Unit = {
  // Process all changed cells
  var cell = didChanged.poll()
  while (cell != null) {
    if (cell.clearDidChange()) {
      processPropagators(cell)
    }
    cell = didChanged.poll()
  }
}
```

### 5. naiveZonk Method Enhancement

```scala
override def naiveZonk(cells: Vector[CIdOf[Cell[?]]])(using Ability): Unit = {
  val currentDepth = incrementRecursionDepth()
  try {
    // Check recursion depth
    if (currentDepth > 5) {
      setDidSomething(true)
      return
    }
    
    // Track processed cells for cycle detection
    val processedCellIds = scala.collection.mutable.Set[String]()
    
    // Implement iteration limits
    var iterationCount = 0
    val maxIterations = 100
    
    // Add cycle detection and clean break mechanism
    case class BreakException() extends RuntimeException
    def break(): Nothing = throw BreakException()
    
    try {
      // Main processing loop with proper cycle detection
      // ...
    } catch {
      case _: BreakException => 
        // Handle break condition
    }
  } finally {
    decrementRecursionDepth()
  }
}
```

### 6. ZonkTask Implementation

```scala
class ZonkTask[Ability](
    c: CIdOf[Cell[?]],
    p: PIdOf[Propagator[Ability]],
    firstFallback: Boolean,
    state: Impl[Ability],
    parentRecursionDepth: Int = 0,
    processedCellIds: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty
)(using more: Ability) extends RecursiveAction {
  override def compute(): Unit = {
    // Skip if recursion is too deep
    if (parentRecursionDepth > 3) {
      setDidSomething(true)
      return
    }
    
    // Implement proper exception handling
    try {
      // Process task...
    } catch {
      case _: BreakException => // Handle break
      case e: Exception => // Handle other exceptions
    }
    
    // Filter out already processed cells to prevent cycles
    // ...
  }
}
```

## Key Principles to Follow

1. **Thread Safety**: Use atomic operations and concurrent collections consistently
2. **Cycle Detection**: Always track processed cells to prevent infinite recursion
3. **Depth Control**: Limit recursion depth to prevent stack overflow
4. **Clean Break Mechanism**: Implement a clean way to break out of loops
5. **Change Tracking**: Always track and propagate changes properly

## Type Compatibility Notes

When implementing these changes, pay special attention to:

1. The relationship between HoldCell/HoldPropagator and CIdOf/PIdOf types
2. Proper typing for Ability parameters
3. Consistent handling of generic type parameters

Following these guidelines should result in a working multithreaded propagator implementation.