const std = @import("std");

// Use a balanced binary search tree (specifically a Red-Black Tree or AVL Tree) as the underlying data structure.
// This approach offers several advantages:

// Efficient operations:

// O(log n) for insertion, deletion, and lookup
// Natural support for ordered traversal
// Good performance for union, intersection, and difference operations

// Memory efficiency compared to hash-based alternatives
// Guaranteed ordering, which is useful for many set operations
// No need for hash functions, which simplifies implementation for custom types
