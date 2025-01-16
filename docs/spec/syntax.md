# Language Syntax Specification

## Lexical Structure

### Keywords

The following are reserved keywords in the language:

```
alias,
as,
else,
end,
exposing,
foreign,
hiding,
if,
in,
include,
infixl,
infixn,
infixr,
let,
match,
module,
on,
open,
renaming,
then,
to,
type,
using,
when,
```

### Identifiers

Identifiers follow these rules:

- Lower identifiers start with a lowercase letter or underscore
- Upper identifiers start with an uppercase letter
- After the first character, may contain letters, numbers, and underscores
- May optionally end with a question mark
- Cannot be keywords

Examples:
```
valid_name
_internal
MyType
is_valid?
calculate123
```

### Literals

#### Integers

- Decimal integers: `42`, `1_000_000`
- Binary integers: `0b1010`, `0b1111_0000`
- Octal integers: `0o52`, `0o755`
- Hexadecimal integers: `0xFF`, `0x1A_2B_3C`
- Underscores are allowed between digits for readability

#### Floating Point 

- Basic decimal: `3.14`, `0.5`, `42.0`
- Scientific notation: `1.23e4`, `1.23e-4`
- Underscores allowed for readability: `1_000.000_1`

#### Characters and Strings

- Character literals: `'a'`, `'\n'`, `'\u0061'`
- String literals: `"hello"`, `"line\nbreak"`
- Multiline strings: `"""multi
                        line
                        string"""`
                        
* Note: Multiline string literals have no escapes and can span across multiple lines. 
* Note: Allowed escape sequences for strings are 
- `\\` (backslash)
- `\"` (double quote)
- `\n` (newline)
- `\t` (tab)
- `\r` (carriage return)
- `\b` (backspace)
- `\uNNNNNN` (unicode) where N is a hexadecimal value
* Note: Allowed escape sequences for characters are
- `\\` (backslash)
- `\'` (single quote)
- `\n` (newline)
- `\t` (tab)
- `\r` (carriage return)
- `\uNNNNNN` (unicode) where N is a hexadecimal value

### Operators

#### Arithmetic

- Integer: `+`, `-`, `*`, `/`
- Float: `+.`, `-.`, `*.`, `/.`
- Exponentiation: `**`

#### Comparison

- Equality: `==`, `/=`
- Ordering: `<`, `<=`, `>`, `>=`

#### Logical

- And: `&&`
- Or: `||`

#### Function

- Composition: `>>`, `<<`
- Pipe: `|>`, `<|`
- Lambda: `\`

#### Other

- Range: `..`
- String Concatenation: `<>`
- List Concatenation: `++`

## Comments

- Single line comments start with `#`
- Documentation comments start with `##`
- Comments continue until end of line

Example:
```
# This is a regular comment
## This is a documentation comment
```

## Special Syntax Elements

### Holes

- Typed holes: `?` (for development/debugging)

### Delimiters

- Parentheses: `(`, `)`
- Brackets: `[`, `]`
- Braces: `{`, `}`
- Other: `,`, `.`, `:`

#### Arrows

- Simple arrow: `->`
- Double arrow: `=>`

#### Special Symbols

- Underscore: `_` (for pattern matching)
- Pipe: `|` (for pattern matching and types)

### Operator Precedence

Operators are listed in descending order of precedence:
1. Function application (highest)
2. Unary operators (`-`)
3. Float exponentiation (`**`)
4. Integer and float multiplication/division (`*`, `/`, `*.`, `/.`)
5. Integer and float addition/subtraction (`+`, `-`, `+.`, `-.`)
6. Cons operator (`::`)
7. String concatenation (`<>`)
8. List concatenation (`++`)
9. Comparison operators (`==`, `/=`, `<`, `<=`, `>`, `>=`)
10. Logical AND (`&&`)
11. Logical OR (`||`)
12. Function composition (`>>`, `<<`)
13. Pipe operators (`|>`, `<|`)
14. Pattern match/lambda arrow (`=>`) 
15. Assignment (`=`) (lowest)

## Conditional Statements

### Syntax
The syntax for conditional statements is:
```mox
if <condition> then
  <expression>
else if <condition> then
  <expression>
else
  <expression>
```
