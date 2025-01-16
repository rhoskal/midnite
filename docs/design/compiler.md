# Compiler Design Philosophy

## Core Design Tenets

1. **Code aesthetics matter**
   - The language syntax is designed for visual clarity and consistency
   - Formatting rules are opinionated but flexible where it matters
   - Code should be naturally readable and self-documenting

2. **Compilers are tools for improvement**
   - Error messages are clear, actionable, and educational
   - Warnings highlight potential improvements, not just problems
   - The compiler suggests fixes and alternatives when possible

3. **Embrace failure management**
   - Error handling is a first-class concern
   - The type system helps prevent common runtime failures
   - Safety features are built-in but can be opted out of when needed

4. **Readability over cleverness**
   - Clear, straightforward code is preferred over terse, complex solutions
   - Language features discourage overly complex abstractions
   - Documentation and maintainability are prioritized

5. **Avoid false compromises**
   - Features are fully implemented rather than partially supported
   - The language makes opinionated choices rather than trying to please everyone
   - We prefer to do fewer things well than many things poorly
