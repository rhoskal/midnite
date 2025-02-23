# Module System

## Overview

Modules are used to group functions around related types. Functions are private by default and must be explicitly added to the export list to be made public.

## Module Syntax

### Basic Module Structure

```mox
module MyModule exposing (..)
    # module contents here
end
```

### Access Control

- Functions are private by default
- Use `exposing (..)` to make items public
- Can expose specific items: `exposing (function1, Type1)`

## Module Operations

### Accessing Module Members

```mox
MyModule.someFunction
MyModule.SomeType
```

### Opening Modules

#### Basic Open

```mox
open MyModule
```

#### Module Alias

```mox
open MyModule.SubModule as Sub
```

#### Selective Import

```mox
open MyModule using (function1, function2)
```

#### Rename Imports

```mox
open MyModule using (oldName as newName)
```

#### Hide Specific Imports

```mox
open MyModule hiding (function1)
```

### Including Modules

Use `include` to import and re-export a module:
```mox
include MyModule
```

## Opaque Types

Modules can be used to create opaque types by controlling what constructors are exposed:

```mox
# In MyModule.mox
module MyModule exposing (MyType)  # Type but not constructors
    type MyType = 
        | Constructor1 
        | Constructor2
end
```

## Best Practices

1. **Module Organization**
   - Group related functions and types together
   - Use meaningful module names
   - Keep modules focused and cohesive

2. **Privacy**
   - Keep implementation details private
   - Only expose what's necessary in the public API
   - Use opaque types when appropriate

3. **Naming**
   - Use descriptive, clear module names
   - Avoid generic names that could conflict
