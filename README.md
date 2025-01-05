# Mox Language Compiler

A new language that combines elements from Elm, Elixir and Ocaml. Features include:

- ML family language (syntax, immutability, static typing, etc)
- 100% Type inference
- Static typing with nominal types
- Garbage collected
- Code is eagerly evaluated
- All functions are curried
- Tail call optimization
- First-class documentation
- Multi-core support
- FFI support with [zig language](https://ziglang.org/)

## Design Tenants

- Code aesthetics matter
- A compiler is there to help you produce better code, not block you
- Things fail and the language should provide safety/guards
- Prefer readable code over clever code
- Most of the time, a compromise leads to the worst outcome.

## Language Features

### Comments

| Feature               | Example                                                          |
|-----------------------|:----------------------------------------------------------------:|
| Documentation Comment | `## Used for generating documentation - interpreted as Markdown` |
| Single line Comment   | `# Normal comment`                                               |

### Built In Types

| Feature   | Example                                       |
|-----------|:---------------------------------------------:|
| Int       | `let x : Int = 42`                            |
| Float     | `let x : Float = 42.0`                        |
| Boolean   | `let x : Bool = True`                         |
| String    | `let x : String = "foo"`                      |
| Char      | `let x : Char = 'c'`                          |
| Unit      | `let x : Unit = Unit`                         |
| Maybe     | `let x : Maybe Int = Some 42`                 |
| Result    | `let x : Result String Int = Ok 42`           |
| Tuple     | `let x : (Int, Int) = (42, 42)`               |
| List      | `let x : List Int = [42]`                     |
| Function  | `let add : Int -> Int -> Int = \x y => x + y` |

### Strings

| Feature              | Example                |
|----------------------|:----------------------:|
| String               | `"foo"`                |
| Multiline String     | `"""foo"""`            |
| String concatenation | `"foo" <> "bar"`       |
| Character            | `'c'`                  |
| String interpolation | `"foo $(bar)"`         |

### Numbers

| Feature                | Example                             |
|------------------------|:-----------------------------------:|
| Integer                | `42`, `-42`, `1_000_000`            |
| Integer operations     | `14 + 1 - 6 * 2 / 3`                |
| Integer modulo         | `42 mod 2`                          |
| Integer exponentiation | `42 ^^ 2`, `pow 42 2`               |
| Float                  | `42.0`, `-42.0`                     |
| Float operations       | `14.0 +. 1.0 -. 6.0 *. 2.0 /. 3.0`  |
| Float exponentiation   | `42.0 ** 2.0`, `Float.pow 42.0 2.0` |
| Hexadecimal            | `0x1F`                              |
| Octal                  | `0o16`                              |
| Binary                 | `0b01110`                           |

### Booleans and Logical Operators

| Feature                 | Example                                  |
|-------------------------|:----------------------------------------:|
| Boolean Values          | `True`, `False`                          |
| Comparison              | `>`, `<`, `>=`, `<=`                     |
| Boolean operations      | `not`, `and`, `or`, `nand`, `nor`, `xor` |
| Boolean infix operators | `\|\|`, `&&`, `^`                        |
| Equality                | `==`, `/=`                               |

### If-Else Expressions

| Feature                 | Example                                  |
|-------------------------|:----------------------------------------:|
| If-Else expressions     | `if condition then a else b`             |

* Note: These are expressions and can be assigned to a variable: `let x = if condition then a else b`

### Functions

| Feature                    | Example                                       |
|----------------------------|:---------------------------------------------:|
| Function definition        | `let add = \x y => x + y`                     |
| Function types             | `let add : Int -> Int -> Int = \x y => x + y` |
| Function calls             | `add 21 21`                                   |
| Partial application        | `let double = mul 2`                          |
| Pipe (apply right)         | `let x = 21 \|> double`                       |
| Pipe (apply left)          | `let x = double <\| increment <\| 3`          |
| Function left composition  | `let nand = not << and`                       |
| Function right composition | `let x = double << increment`                 |

* Note: Functions have implicit returns. The last expression in a block or function definition is the returned value.
* Note: Functions are "data last" so take that into note when using the pipe operators.

### Maps and Sets

| Feature                    | Example                                       |
|----------------------------|:---------------------------------------------:|
| Map                        | `let x = Map ...`                             |
| Set                        | `let x = Set ...`                             |

### Records

| Feature                    | Example                                              |
|----------------------------|:----------------------------------------------------:|
| Record definition          | `type FooBar = { foo : Int, bar : String }`          |
| Record creation            | `let x : FooBar = { foo = 10, bar = "hello" }`       |
| Record access              | `x.foo`                                              |
| Destructuring              | `let {foo, bar} = x`                                 |
| Lens operations            | `:>` (access), `?>` (optional access), `~>` (modify) |

* Note: Record types are nominal rather than structural.

### Variants

Variant types (aka sum types or enums in other languages) model values that may assume one of many known variations.

| Feature                    | Example                                              |
|----------------------------|:----------------------------------------------------:|
| Variant definition         | `type FooBar = \| Foo \| Bar;`                       |
| Variants with args         | `type FooBar = \| Foo String \| Bar Int`             |
| With type parameters       | `type FooBar a = \| Foo a \| Bar a`                  |

### Null Values

Mox does not allow null values. Instead, the `Maybe` type is a built-in variant that represents the presence or absence of a value.

| Feature                      | Example                            |
|------------------------------|:----------------------------------:|
| Definition (already defined) | `type Maybe a = \| None \| Some a` |
| Value that is present        | `let x = Some 42`                  |
| Value that is absent         | `let x = None`                     |

### Errors

When operations can both succeed or fail, a `Result` type is used to model such cases. Use the `Validation` type when you need to accumulate errors.

| Feature                      | Example                              |
|------------------------------|:------------------------------------:|
| Definition (already defined) | `type Result e a = \| Err e \| Ok a` |
| Value that is an error       | `let x = Err "foo!"`                 |
| Value that is a success      | `let x = Ok "bar"`                   |

### Pattern Matching

Pattern matching matches against variants and ensures all cases are covered. Start matching using the `match` keyword:

```mox
match foo on
| Some value => do_something value
| None => error
```

| Feature                    | Example                                               |
|----------------------------|:-----------------------------------------------------:|
| Basic case                 | `\| Some value  => do_something value`                |
| When conditions            | `\| Some value when value > 10 => do_something value` |
| Catch-all case             | `\| _ => do_something`                                |
| Matching lists             | `\| x :: xs => do_something xs`                       |
| Matching records           | `\| {foo: value} => do_something value`               |
| Matching tuples            | `\| (x, y) => do_something x`                         |
| Matching liters            | `\| "Hello" => handle_hello`                          |

### Unit

The special "Unit" value (written Unit) represents something that never has any meaningful value.

| Feature                    | Example         |
|----------------------------|:---------------:|
| Creating a unit            | `let x = Unit`  |

### Modules

Modules are a way to group functions around a type(s). Functions are private by default and therefore must be added to the export list to be made public.

| Feature                             | Example                            |
|-------------------------------------|:----------------------------------:|
| Module creation                     | `module Foo exposing (..) ... end` |
| Module member access                | `Foo.bar`                          |
| Import fuctions into current module | `open Foo`                         |
| Alias the module                    | `open Foo.Bar as Bar`              |
| Import a list of functions          | `open Foo using (xs)`              |
| Rename imports                      | `open Foo renaming (ys to zs)`     |
| Hiding imports                      | `open Foo hiding (zs)`             |
| Import to be re-exported            | `include Foo`                      |

### Types

| Feature                    | Example                                                  |
|----------------------------|:--------------------------------------------------------:|
| Annotations                | `let answer : Int = 42`                                  |
| Type aliases               | `type alias Seconds = Int`                               |
| Opaque Types               | `type FooBar = \| Foo \| Bar`, `module X exposing (Foo)` |

### Partials

| Feature                    | Example                                    |
|----------------------------|:------------------------------------------:|
| Panic!                     | `Partials.panic "roll over and play dead"` |
| Todo                       | `Partials.todo "feelin' lazy"`             |

### Special Operators

| Feature                 | Example                 |
|-------------------------|:-----------------------:|
| Range operator          | `1 .. 6`                |
| Cons operator           | `3 :: [1, 2]`           |

### Bitwise Operations

| Feature      | Example                               |
|--------------|:-------------------------------------:|
| and          | `9 .&. 3`, `Bitwise.and 9 3`          |
| or           | `9 .\|. 3`, `Bitwise.or 9 3`          |
| xor          | `9 .^. 3`, `Bitwise.xor 9 3`          |
| not          | `.~. 2`, `Bitwise.not 2`              |
| shift left   | `1 .>>. 2`, `Bitwise.shift_left 1 2`  |
| shift right  | `1 .<<. 2`, `Bitwise.shift_right 1 2` |

### Side Effects

| Feature                            | Example                                  |
|------------------------------------|:----------------------------------------:|
| Sync Definition (already defined)  | `let log : Eff () String = ...`          |
| Async Definition (already defined) | `let fetch : Aff HttpErr Response = ...` |

### Foreign Function Interface (FFI)

| Feature             | Example                                                     |
|---------------------|:-----------------------------------------------------------:|
| Function Definition | `foreign bitwise_and : Int -> Int -> Int = "c_bitwise_and"` |
| Function wrapper    | `let and : Int -> Int -> Int = bitwise_and`                 |
