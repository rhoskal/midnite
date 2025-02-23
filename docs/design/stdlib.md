# Standard Library

## Core Modules

### Prelude

Core functions and types that are automatically imported.

### Data Types

#### Maybe

Optional values:
```mn
type Maybe(a) = 
    | None 
    | Some(a)
```

#### Result

Error handling:
```mn
type Result(e, a) = 
    | Err(e)
    | Ok(a)
```

#### Validation

Error accumulation:
```mn
type Validation(e, a)
```

#### List

Immutable linked lists:
```mn
type List(a) =
    | Nil
    | a :: List(a)
```

#### NonEmpty

Non-empty collections:
```mn
type NonEmpty(a) =
    | [a]
    | a :: List(a)
```

#### Set

Unique collections:
```mn
type Set ...
```

### Effect System

#### Eff

Synchronous effects handling:
```mn
type Eff(e, a)
```

#### Aff

Asynchronous effects handling:
```mn
type Aff(e, a)
```

### Value Types

#### Boolean

Boolean operations and combinators:
```mn
...
```

#### Float

Floating point operations:
```mn
...
```

#### String

String manipulation and processing:
```mn
...
```

#### Unit

Unit type and operations:
```mn
type Unit = Unit
```

### Data Structures

#### Tuple

Tuple types and operations:
```mn
...
```

### Type Classes

#### Comparable

Interface for orderable types:
```mn
...
```

### Utilities

#### Bitwise

Low-level bitwise operations:
```mn
...
```

### Module Organization

- Core types are exposed in the prelude
- Specialized functionality in focused modules
- Effects are handled by dedicated modules
- Utilities are organized by domain

### Usage Guidelines

1. **Importing**
   ```mn
   # Most common modules available in prelude
   # Import others explicitly
   open List
   open Set as S
   ```

2. **Effect Handling**
   ```mn
   # Synchronous effects
   let io() : Eff(IO, a)

   # Asynchronous effects
   let async() : Aff(Error, a)
   ```

3. **Error Handling**
   ```mn
   # Optional values
   let find(list : List(a), x : a) -> Maybe(a)

   # Operations that can fail
   let parse(str : String) -> Result(Error, Int)

   # Accumulating errors
   let validate(value : a) -> Validation(Error, b)
   ```

### Best Practices

1. **Module Usage**
   - Use prelude for common operations
   - Import specific modules for specialized needs
   - Prefer qualified imports for clarity

2. **Type Selection**
   - Use `Maybe` for optional values
   - Use `Result` for operations that can fail
   - Use `Validation` for error accumulation
   - Use `NonEmpty` when guaranteed non-emptiness is required

3. **Effects**
   - Keep effects at the edges of your system
   - Use appropriate effect type (`Eff` vs `Aff`)
   - Handle effects explicitly
