# Error Handling

## Overview

Mox takes a systematic approach to error handling through the type system. There are no null values or exceptions. Instead, the language provides three core types for handling different error scenarios:
- `Maybe` - For optional values
- `Result` - For operations that can fail
- `Validation` - For accumulating multiple errors

## Maybe Type

Used when a value might or might not be present.

```mox
type Maybe(a) = 
    | None 
    | Some(a)

# Examples
let find_user(username : String) -> Maybe(User) =
    ...

let parse_age(input : String) -> Maybe(Int) =
    ...

# Pattern matching with Maybe
match user on
| None => show_login_prompt
| Some(u) => greet_user(u)
```

## Result Type

Used when an operation can fail with a specific error.

```mox
type Result(e, a) =
    | Err(e)
    | Ok(a)

# Examples
let divide(dividend : Int, divisor : Int) -> Result(String, Int) =
    if divisor == 0 then
        Err("Division by zero")
    else
        Ok(dividend / divisor)

let read_file(path : String) -> Result(IOError, String) =
    ...

# Pattern matching with Result
match result on
| Err(msg) => handle_error(msg)
| Ok(value) => process_value(value)
```

## Validation Type

Used when you need to accumulate multiple errors rather than fail fast.

```mox
# Example: Form validation
type ValidationError =
    | InvalidEmail(String)
    | InvalidAge(Int)
    | PasswordTooShort

let validate_form(form : Form) -> Validation(ValidationError, ValidForm) =
    ...
```

## Partial Functions

The language provides explicit ways to handle incomplete functions:

```mox
# Explicitly mark incomplete code
let todo(message : String) -> a =
    Partials.todo(message)

# Mark impossible cases
let impossible(message : String) -> a =
    Partials.panic(message)
```

## Error Handling Guidelines

1. **Explicit Error Types**
   - Use descriptive custom error types
   - Error types should be part of the public API
   - Document possible error conditions

2. **Fail Fast vs Error Accumulation**
   - Use `Result` when you want to fail on first error
   - Use `Validation` when collecting all errors is important

3. **Error Context**
   - Include relevant context in error types
   - Consider location/source of errors
   - Make errors actionable

## Pattern Matching with Error Types

```mox
# Combining Maybe values
match (first_name, last_name) on
| (Some(first), Some(last)) => full_name(first, last)
| _ => default_name

# Combining Results
match (validate_email, validate_age) on
| (Err(e), _) => handle_error(e)
| (_, Err(e)) => handle_error(e)
| (Ok(email), Ok(age)) => create_user(email, age)
```

## Error Propagation

```mox
# Using pipe operators with error handling
input
|> validate_email
|> Result.map(fn(x) => validate_password(x))
|> Result.map(fn(x) => create_user(x))
```

## FFI Error Handling

When interfacing with Zig through FFI:
- Zig errors must be mapped to appropriate Mox error types
- FFI boundaries should have clear error contracts
- Consider using custom error types for FFI-specific errors
