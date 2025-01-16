## Type Definitions

The language implements a Hindley-Milner type system with:
- Parametric polymorphism
- Type inference
- Algebraic data types

## Basic Types

The language provides these built-in types:

```
Int     -- 64-bit signed integer
Float   -- 64-bit floating point
Char    -- Unicode character
String  -- UTF-8 string
Bool    -- Boolean true/false
Unit    -- Single value type ()
```

## Type Constructors

### Records

```
type Person = 
    { name : String
    , age : Int
    , email : Maybe String
    }
```

### Sum Types

```
type Maybe a =
    | None
    | Some a

type Result e a =
    | Err e
    | Ok a
```

### Type Aliases

```
type alias UserId = String
type alias Age = Int
type alias EmailAddress = String
```

## Type Classes

Not sure if these will be supported and if so, to what extent.

## Effect System

Effects are tracked in the type system:

```
# Pure function type
let map : (a -> b) -> List a -> List b

# Function with sync effect
let read_file : String -> Eff (Result Error String)

# Function with async effect
let read_file : String -> Aff (Result Error String)
```

## Type Inference

The type system implements bidirectional type inference:
- Expressions are analyzed to infer their type
- Type annotations can provide additional context
- Most general type is inferred when possible

Example:
```
# Inferred: a -> a
let identity = \x => x

# Inferred: Int -> Int -> Int 
let add = \x y => x + y

# Annotated
let repeat : Int -> a -> List a = \n x => ...
```
