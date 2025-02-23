# Type Inference

## Overview

Midnite implements complete type inference using an extended Hindley-Milner type system. Type annotations are optional but supported for documentation and clarity.

## Core Principles

- Every expression has a type that can be inferred
- Type annotations are never required
- The most general (polymorphic) type is inferred
- Inference is predictable and deterministic

## Type Inference Examples

### Basic Inference

```mn
# Inferred: (Int, Int) -> Int
let add = 
    fn(x, y) => x + y

# Inferred: List a -> Int
let length = 
    fn(xs) => 
        match xs on
        | [] => 0
        | x :: rest => 1 + length(rest)

# Inferred: (a, List a) -> List a
let prepend = 
    fn(x, xs) => x :: xs
```

### Record Type Inference

```mn
# Inferred: { name : String, age : Int } -> String
let get_name(record) = record.name

# Inferred: { name : String, age : Int }
let person = { name = "Alice", age = 30 }
```

### Function Composition

```mn
# Inferred: ((b -> c), (a -> b), a) -> c
let compose(f, g, x) =
    f(g(x))

# Inferred: (List(a), (a -> b)) -> List(b)
let map(xs, f) = 
    match xs on
    | Nil => Nil
    | x :: rest => f(x) :: map(rest, f)
```

### Generic Types

```mn
# Inferred: (Maybe(a), b) -> b
let maybe(opt, default) =
    match opt on
    | None => default
    | Some(x) => value(x)

# Inferred: (Result(e, a), (a -> b), (e -> b)) -> b
let either(result, success, error) =
    match result on
    | Err(err) => error(err)
    | Ok(value) => success(value)
```

## Type Annotations

While never required, type annotations can be added for clarity:

```mn
# Inferred: (Int) -> Int  
let increment(x) = x + 1  

# Inferred: (List(a), (a -> b)) -> List(b)  
let map(xs, f) =  
    match xs on  
    | Nil => Nil
    | x :: rest => f(x) :: map(rest, f)  
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

```mn
# Types are inferred through the pipeline
data
|> transform    # a -> b
|> validate     # b -> Result(e, c)
|> process      # c -> d
```

### Higher-Order Function Inference

```mn
# Type parameters are properly propagated
let apply(x, f) = f(x)
let double(n) = n * 2
let result() = apply(21, double) # Inferred: Int
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
