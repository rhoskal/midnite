# Effects System

## Overview

Mox differentiates between pure and effectful computations at the type level. The language provides two core effect types:
- `Eff` - For synchronous effects
- `Aff` - For asynchronous effects

## Synchronous Effects (Eff)

### Type Definition

```mox
type Eff(e, a)
```

Where:
- `e` is the effect type (IO, State, etc.)
- `a` is the result type

### Common Effects

```mox
# IO operations
let read_line() -> Eff(IO, String) =
    ...

let print(str : String) -> Eff(IO, Unit) =
    ...

# State operations
let get_state() -> Eff(State(s), s) =
    ...

let set_state(state : s) -> Eff(State(s), Unit) =
    ...
```

## Asynchronous Effects (Aff)

### Type Definition

```mox
type Aff(e, a)
```

Where:
- `e` is the error type
- `a` is the result type

### Common Async Operations

```mox
let fetch(url : String) -> Aff(HttpError, Response) =
    ...

# Database operations
let query(sql : String) -> Aff(DbError, Result) =
    ...
```

## Effect Composition

Effects can be composed using do-notation or operators:

```mox
# Sequential composition
let program() = 
    Eff.and_then(read_line, fn(name) => print("Hello " <> name <> "!"))

# Parallel composition (for Aff)
let parallel() = 
    Aff.all([task1, task2, task3])
```

## Pure vs Effectful Code

The type system enforces separation between pure and effectful code:

```mox
# Pure function
let double(x : Int) -> Int =
    x * 2

# Effectful function
let read_and_double() : Eff(IO, Int) =
    Eff.map(read_line, fn(x) => double(Int.parse(x)))
```

## Effect Handlers

[Documentation needed on how effects are handled/interpreted]

## FFI and Effects

When using FFI with Zig:
```mox
# FFI function that has effects
foreign print_line(str : String) -> Eff(IO, Unit) = "zig_print_line"
```

## Error Handling in Effects

Effects integrate with Mox's error handling types:

```mox
# Synchronous effect that might fail
let parse(input : String) -> Eff(IO, Result(ParseError, Int)) =
    ...

# Asynchronous effect with built-in error type
let fetch(url : Url) -> Aff(HttpError, Response) =
    ...
```

## Best Practices

1. **Effect Isolation**
   - Keep pure code separate from effectful code
   - Push effects to the edges of your system
   - Compose pure functions within effectful contexts

2. **Error Handling**
   - Use appropriate error types for each effect
   - Handle all potential failures
   - Consider using `Validation` for accumulating errors

3. **Testing**
   - Mock effects for testing
   - Test pure business logic separately
   - Use effect tracking for integration tests
