# Type Inference

## Overview

Mox implements complete type inference using an extended Hindley-Milner type system. Type annotations are optional but supported for documentation and clarity.

## Core Principles

- Every expression has a type that can be inferred
- Type annotations are never required
- The most general (polymorphic) type is inferred
- Inference is predictable and deterministic

## Type Inference Examples

### Basic Inference

```mox
# Inferred: Int -> Int -> Int
let add = \x y => x + y

# Inferred: List a -> Int
let length = \xs => 
    match xs on
    | [] => 0
    | x :: rest => 1 + length rest

# Inferred: a -> List a -> List a
let prepend = \x xs => x :: xs
```

### Record Type Inference

```mox
# Inferred: { name : String, age : Int } -> String
let get_name record = record.name

# Inferred: { name : String, age : Int }
let person = { name = "Alice", age = 30 }
```

### Function Composition

```mox
# Inferred: (b -> c) -> (a -> b) -> a -> c
let compose = \f g x => f (g x)

# Inferred: (a -> b) -> List a -> List b
let map = \f xs => 
    match xs on
    | [] => []
    | x :: rest => f x :: map f rest
```

### Generic Types

```mox
# Inferred: Maybe a -> b -> b -> b
let maybe = \default value opt =>
    match opt on
    | None => default
    | Some x => value x

# Inferred: Result e a -> (a -> b) -> (e -> b) -> b
let either = \success error result =>
    match result on
    | Err err => error err
    | Ok value => success value
```

## Type Annotations

While never required, type annotations can be added for clarity:

```mox
let increment : Int -> Int = \x => x + 1

let map : (a -> b) -> List a -> List b = \f xs => 
    match xs on
    | [] => []
    | x :: rest => f x :: map f rest
```

## Inference Limitations

1. **Record Fields**
   - Record field types are inferred from usage
   - Record types are nominal, not structural

2. **Type Classes**
   - [Documentation needed on type class inference]

3. **Effect Types**
   - Effect types are inferred from operations
   - Effect composition follows inference rules

## Common Patterns

### Pipeline Inference

```mox
# Types are inferred through the pipeline
data
|> transform    # a -> b
|> validate     # b -> Result e c
|> process      # c -> d
```

### Higher-Order Function Inference

```mox
# Type parameters are properly propagated
let apply f x = f x
let double n = n * 2
let result = apply double 21  # Inferred: Int
```

## Implementation Notes

The type inference algorithm follows these steps:

1. Generate type constraints from AST
2. Unify constraints
3. Resolve to most general type
4. Apply defaulting rules if needed

## Best Practices

1. **Type Annotations**
   - Add annotations for public API functions
   - Use annotations to document constraints
   - Add annotations for complex functions

2. **Type Variables**
   - Use meaningful type variable names
   - Keep polymorphic functions generic
