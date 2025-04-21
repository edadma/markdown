package io.github.edadma.markdown

// Represents a processed character with its context
case class Cursor(
    char: Char,        // The character (possibly transformed)
    pos: Int,          // Position in original input
    line: Int,         // Line number (0-based)
    column: Int,       // Column number (0-based)
    isLiteral: Boolean, // Whether this should be treated literally (not as syntax)
)

// Sentinel value for end of input
object EndOfInput extends Cursor('\u0000', -1, -1, -1, false)
