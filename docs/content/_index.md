---
title: markdown
splash: true
heroTitle: A fast, minimal Scala 3
heroHighlight: Markdown parser
summary: Full CommonMark 0.31.2 compliance — 652 / 652 spec tests passing — across JVM, Scala.js, and Scala Native. Zero runtime dependencies, opt-in extensions for tables, math, callouts, footnotes, emoji, and more.
---

## What it is

`io.github.edadma.markdown` is a Scala 3 library that turns CommonMark text into HTML, an XML AST, or a structured `Document` you can walk yourself.

```scala
import io.github.edadma.markdown.*

renderToHTML("# Hello, CommonMark!\n\nThis is **bold**, *italic*, and `code`.")
// → "<h1>Hello, CommonMark!</h1>\n<p>This is <strong>bold</strong>, ..."
```

The full surface — config knobs, AST node types, custom renderers — is documented in [Reference](/reference/).

## Why this one?

- **100 % CommonMark 0.31.2.** Every one of the 652 spec tests passes on every platform. No "almost compliant" surprises.
- **Cross-platform from one source.** JVM, Scala.js, Scala Native — same API, no platform-specific deps. The npm package `@edadma/markdown` is just the linked Scala.js output with TypeScript typings on top.
- **Lean.** Zero runtime dependencies (`cross_platform`, `logger`, `dllist` are author's own micro-libraries — no third-party transitive surface).
- **Pluggable extensions.** Tables, strikethrough, task list items, footnotes, math, callouts, smart punctuation, emoji, attribute spans, definition lists, doc-tags — opt in only what you need.
- **AST access.** When HTML isn't what you want, parse to `Document` and walk the tree yourself. The same nodes drive HTML and XML emitters internally.

## Cross-platform

| Target                              | Status |
|-------------------------------------|--------|
| JVM (Scala 3.8.3, Java 17+)         | ✓      |
| Scala.js 1.21.0 (Node 20+)          | ✓      |
| Scala Native 0.5.11                 | ✓      |
| npm `@edadma/markdown` (linked JS)  | ✓      |

652 / 652 CommonMark spec tests pass on every platform.

## Try it live

The [Dingus](https://edadma.github.io/dingus/) is a browser sandbox running this exact library — paste markdown in, see HTML come out, no install required.

## Where to go next
