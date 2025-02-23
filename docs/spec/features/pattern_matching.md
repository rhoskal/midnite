# Pattern Matching Specification

## Overview

Pattern matching is a core feature of the language that allows you to route a value through a series of patterns, performing different operations based on its structure. The `match` expression operates directly on a value, testing it against patterns to destructure its contents and control program flow.
Rather than nesting conditionals or manually destructuring data, pattern matching provides a declarative way to express "when the value matches this shape, do this thing." This makes code both more concise and easier to reason about, especially when working with complex data structures.

## Pattern Types

### Literal Patterns

```
match x on
| 0 => "zero"
| 1 => "one"
| _ => "many"

match c on
| 'a' => "first letter"
| 'z' => "last letter"
| _ => "other letter"
```

### Constructor Patterns

```
match maybe on
| None => "empty"
| Some(value) => "got " <> value

match result on
| Err(msg) => "error: " <> msg
| Ok(value) => "success: " <> value
```

### Record Patterns

```
match person on
| { name, age } => "Alice is " <> Maybe.with_default(String.from_int(age), 0)
| _ -> "someone else"
```

### List Patterns

```
match list on
| Nil => "empty"
| [x] => "singleton: " <> Maybe.with_default(String.from_int(x), 0)
| x :: xs => "cons: " <> Maybe.with_default(String.from_int(x), 0)
```

### Guard Patterns

```
match n on
| x when x < 0 => "negative"
| x when x > 0 => "positive"
| _ => "zero"
```

## Pattern Matching Rules

1. Patterns are matched in order from top to bottom
2. Patterns must be exhaustive (cover all cases)
3. The compiler warns about unreachable patterns
4. Variables in patterns introduce new bindings
5. The underscore pattern (_) matches anything
