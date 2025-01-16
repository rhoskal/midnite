# Language Semantics

## Evaluation Model

### Core Properties

- Pipeline oriented (data transformation focus)
- Eagerly evaluated (strict evaluation)
- Immutable data structures
- All functions are curried
- No null values
- Tail call optimization
- Static typing with nominal types

### Pipeline-Oriented Programming

Pipeline-oriented programming emphasizes data transformation through a series of discrete steps. Functions are designed to be composable and "data-last" to support natural pipeline construction using the pipe operators (`|>` and `<|`).

Example:
```mox
some_data
|> transform
|> validate
|> process
|> format
```

### Variable Binding

- Variables are immutable
- Shadowing is not allowed
- Variables must be initialized before use
- Lexical scoping rules apply

### Function Evaluation

- Functions are curried by default
- Arguments are evaluated left-to-right
- Last expression in a block is the implicit return value
- Partial application is supported
- Function composition preserves type safety

### Pattern Matching

- Patterns are evaluated top-to-bottom
- Must be exhaustive (all cases covered)
- More specific patterns must come before more general ones
- Guards provide additional boolean conditions
- Irrefutable patterns always match

### Type System

- Static typing throughout
- 100% type inference (no type annotations required)
- Nominal typing for records
- Generic type parameters are possible
- Type safety is guaranteed at compile time

### Error Handling

- No exceptions or null values
- `Maybe` type for optional values
- `Result` type for operations that can fail
- `Validation` type for error accumulation

### Side Effects

- Effects are tracked in the type system
- Synchronous effects use `Eff` type
- Asynchronous effects use `Aff` type
- Pure functions are separate from effectful code

### Memory Management

- Garbage collected
- No manual memory management required
- No explicit allocation/deallocation

### Foreign Function Interface

- Direct FFI support with Zig
- Type safety maintained across FFI boundary
- Foreign functions must be explicitly marked

### Operator Precedence and Associativity

[Detailed operator precedence rules would go here]

### Module System

- Private by default
- Explicit exports
- No circular dependencies
- Module loading is deterministic

### Multi-threading

- Safe multi-core support
- Thread safety guaranteed by type system
- No shared mutable state

## Examples

### Pattern Matching Semantics

```mox
match value on
| pattern1 when guard => expression1  # Evaluated first if guard is true
| pattern2 => expression2             # Evaluated if pattern1 didn't match
| _ => default_expression             # Catch-all case
```

### Function Application

```mox
# These are equivalent:
add 1 2
(add 1) 2      # Due to currying
1 |> add 2     # Right pipe
add 2 <| 1     # Left pipe
```

### Effect Handling

```mox
# Pure function
let pure : Int -> Int

# Function with effects
let effectful : Int -> Eff () Int

# Async function with possible errors
let async : String -> Aff HttpErr Response
```

## Control Flow

### Conditional Statements

The `if` expression evaluates its condition to a boolean value. If true, the 
`then` branch is evaluated. Otherwise, the next `else if` condition is evaluated
or the `else` branch is taken if present.

#### Type Rules

- The condition must evaluate to a boolean type
- All branches must return values of the same type
- The `else` branch is requried
