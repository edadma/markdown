# Scala CommonMark Implementation Project

## Project Overview

The goal is to create a cross-platform, CommonMark-compliant Markdown processor in Scala. The implementation should:

- Be fully compliant with the CommonMark specification
- Support GitHub Flavored Markdown extensions
- Use a robust, extendable architecture
- Leverage Scala's type safety and functional programming
- Work across JVM, Scala.js, and potentially Scala Native platforms

## Architectural Approach

### Core Design Principles

1. **Two-Phase Parsing**: Following CommonMark's approach of block parsing followed by inline parsing
2. **AST-Based**: Using an intermediate Abstract Syntax Tree between parsing and rendering
3. **Hand-Coded Parser**: Using specialized, hand-crafted parsing algorithms rather than parser combinators
4. **TDD Methodology**: Using spec tests to drive implementation
5. **Debug-First**: Comprehensive debug logging throughout parsing stages
6. **Incremental Implementation**: Starting with paragraphs and building up incrementally

### Component Structure

```
MarkdownProcessor
  |
  +-- InputReader        // Character-level processing
  |
  +-- BlockParser        // Handles document structure
  |     |
  |     +-- Block Processors (Paragraph, Heading, List, etc.)
  |
  +-- InlineParser       // Handles formatting within blocks
  |     |
  |     +-- Delimiter Processor (emphasis, etc.)
  |     +-- Link Processor
  |     +-- Code Span Processor
  |
  +-- HtmlRenderer       // Renders AST to HTML
  |
  +-- Extension System   // For GFM and custom extensions
```

### Character Processing Design

A key insight is to use a stream of cursor objects to handle character-level processing:

```scala
// Represents a processed character with its context
case class Cursor(
  char: Char,          // The character (possibly transformed)
  pos: Int,            // Position in original input
  line: Int,           // Line number (0-based)
  column: Int,         // Column number (0-based)
  isLiteral: Boolean   // Whether this should be treated literally (not as syntax)
)

// Sentinel value for end of input
object EndOfInput extends Cursor('\u0000', -1, -1, -1, false)

class InputReader(input: String) {
  private val normalizedInput = normalizeInput(input)
  
  // Main public stream of cursors
  val stream: LazyList[Cursor] = processToCursors(normalizedInput)
  
  // Helper methods for normalization, cursor generation, etc.
}
```

Key features of this approach:
- The `isLiteral` flag indicates when a character should be treated literally (not as syntax)
- It handles escape sequences and entity references consistently
- It tracks line and column positions for error reporting
- It normalizes line endings as per the spec
- It replaces U+0000 with U+FFFD (replacement character)
- It uses a lazy list for efficient processing of large documents

### Data Model

```scala
// AST Structure
sealed trait Node
case class Document(children: List[Block]) extends Node

// Block nodes
sealed trait Block extends Node
case class Paragraph(inlines: List[Inline]) extends Block
case class Heading(level: Int, inlines: List[Inline]) extends Block
case class BlockQuote(children: List[Block]) extends Block
case class List(listType: ListType, items: List[ListItem]) extends Block
case class ListItem(children: List[Block]) extends Block
// Other block types...

// Inline nodes
sealed trait Inline extends Node
case class Text(content: String) extends Inline
case class Emphasis(children: List[Inline]) extends Inline
case class Strong(children: List[Inline]) extends Inline
case class Link(destination: String, title: Option[String], children: List[Inline]) extends Inline
// Other inline types...
```

## Testing Strategy

### Test-Driven Development

- Using ScalaTest as the testing framework
- Parsing CommonMark spec examples into test cases
- Creating dedicated tests for character-level processing
- Categorizing tests by feature
- Implementing incrementally, focusing on one feature at a time

### Character-Level Testing

Focused tests for the InputReader should verify:
- Basic character processing
- Escape sequence handling (`\>`, `\*`, etc.)
- Line and column tracking
- Line ending normalization
- Entity reference handling (`&amp;`, `&#35;`, etc.)
- Null character replacement
- Multiple levels of escaping
- Tab expansion in contexts where it matters

```scala
// Example test cases
"InputReader" should "process basic characters correctly" in {
  val input = "abc"
  val reader = new InputReader(input)
  val stream = reader.stream.toList
  
  // Verify character properties
}

it should "handle escaped punctuation" in {
  val input = "\\>\\*\\[\\`"
  val reader = new InputReader(input)
  val stream = reader.stream.toList
  
  // Verify escape handling
}

it should "recognize and process HTML entities" in {
  val input = "&amp; &lt; &gt;"
  val reader = new InputReader(input)
  val stream = reader.stream.toList
  
  // Verify entity handling
}
```

### Debug Logging Rules

1. Log method entries and exits with parameters and return values
2. Log state changes with before/after snapshots
3. Provide context in logs for easier debugging
4. Use hierarchical logging to show parsing flow
5. Save logs for failing tests

## Key Algorithms

### Character Processing

1. **Input Normalization**:
   - Replace U+0000 with U+FFFD
   - Normalize line endings (CR, LF, CRLF) to LF

2. **Cursor Generation**:
   - Process escape sequences (backslash followed by ASCII punctuation)
   - Process entity references
   - Track line and column positions
   - Mark characters as literal when appropriate

3. **Tab Handling**:
   - Context-sensitive tab expansion (4-space tab stops) in block structure contexts
   - Preserve tabs in other contexts

### Block Parsing

The block parsing follows a line-by-line approach:

1. Check each line for continuation of existing blocks
2. Close blocks that can't continue with the current line
3. Check for new block starts
4. Add content to the current open block

### Inline Parsing (Complex Areas)

The inline parsing has several challenging areas:

1. **Delimiter Processing**: Handling emphasis and strong emphasis with rules for flanking delimiters
2. **Link Resolution**: Processing links with potential nesting and reference resolution
3. **Code Spans**: Finding matching backtick sequences

## Implementation Strategy

1. **Implement the InputReader and its tests**
2. **Implement basic block parsing for paragraphs only**
3. **Implement simple inline parsing (text, escapes)**
4. **Tackle complex inline parsing with emphasis rules**
5. **Add remaining block types**
6. **Implement extended features and GFM**

## Next Steps

1. **Complete the InputReader implementation**
2. **Thoroughly test character-level processing**
3. **Create AST data structures**
4. **Implement block parsing for paragraphs**
5. **Begin work on the inline parsing algorithm**

## Open Questions/Areas

- Optimal implementation of tab expansion in block structure contexts
- Full entity reference resolution (full set of HTML5 entities)
- Implementation of the delimiter processing algorithm
- Reference link resolution
- Extension mechanisms for GitHub Flavored Markdown