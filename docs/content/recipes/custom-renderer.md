---
title: Writing a custom renderer
summary: Walk the AST yourself and emit your own format — plain text, JSON, terminal ANSI, anything.
weight: 20
---

The built-in renderers are not magic. `renderToHTML` and `renderToXML` are pattern matches over `Block` and `Inline`. To target a different format, you write the same shape of pattern match for your output.

## The walker pattern

Every `Block` either holds child blocks (`BlockQuote`, `ListBlock`, …) or a `List[Inline]` (`Paragraph`, `Heading`, …). Every `Inline` either holds child inlines (`Emphasis`, `Strong`, `Link`, …) or carries a leaf value (`Text`, `CodeSpan`, …). You walk recursively.

## Example: a plain-text dumper

A renderer that emits exactly what `plainText` gives you for inlines, but with sensible block-level whitespace:

```scala
import io.github.edadma.markdown.*

def renderToText(node: Node): String =
  val out = new StringBuilder

  def block(b: Block): Unit = b match
    case Paragraph(inlines)              => inlines.foreach(inline); out += '\n'; out += '\n'
    case Heading(level, inlines, _)      =>
      out ++= "#" * level; out += ' '
      inlines.foreach(inline); out += '\n'; out += '\n'
    case Code(content, _, _, _)          => out ++= content; out += '\n'; out += '\n'
    case BlockQuote(children)            => children.foreach(b => { out ++= "> "; block(b) })
    case ThematicBreak()                 => out ++= "----\n\n"
    case ListBlock(data, items)          =>
      items.zipWithIndex.foreach { case (item, i) =>
        out ++= (if data.isOrdered then s"${i + 1}. " else "- ")
        item.content.foreach(block)
      }
    case HTMLBlock(c)                    => out ++= c; out += '\n'
    case _                               => () // table / callout / footnote / etc.

  def inline(i: Inline): Unit = i match
    case Text(c)                  => out ++= c
    case SoftLineBreak()          => out += ' '
    case HardLineBreak()          => out += '\n'
    case CodeSpan(c)              => out += '`'; out ++= c; out += '`'
    case Emphasis(xs)             => xs.foreach(inline)
    case Strong(xs)               => xs.foreach(inline)
    case Strikethrough(xs)        => xs.foreach(inline)
    case Link(_, _, xs)           => xs.foreach(inline)
    case Image(_, _, xs, _)       => xs.foreach(inline)
    case AutoLink(_, text)        => out ++= text
    case _                        => ()

  node match
    case Document(children) => children.foreach(block)
    case b: Block           => block(b)
    case i: Inline          => inline(i)

  out.toString
```

Use it:

```scala
val md = "# Title\n\nA paragraph with **bold**.\n"
val txt = renderToText(parseDocumentContent(md))
// → "# Title\n\nA paragraph with bold.\n\n"
```

## Example: terminal ANSI

The same pattern with ANSI escape sequences instead of HTML tags. Just substitute the inline cases:

```scala
case Strong(xs)        => out ++= "[1m"; xs.foreach(inline); out ++= "[22m"
case Emphasis(xs)      => out ++= "[3m"; xs.foreach(inline); out ++= "[23m"
case CodeSpan(c)       => out ++= "[7m"; out ++= c; out ++= "[27m"
case Link(_, _, xs)    => out ++= "[4m"; xs.foreach(inline); out ++= "[24m"
```

Same for headings (use bright color + bold), code blocks (block-level reverse-video), thematic breaks (a row of `─`).

## Filtering before rendering

Walking the AST also lets you *transform* it before rendering. Strip raw HTML for safety, demote heading levels, rewrite link destinations:

```scala
def stripRawHtml(doc: Document): Document =
  def b(block: Block): Block = block match
    case HTMLBlock(_)               => Paragraph(Nil)            // drop it
    case Paragraph(inlines)         => Paragraph(inlines.filterNot(_.isInstanceOf[RawHTML]))
    case Heading(l, inlines, a)     => Heading(l, inlines.filterNot(_.isInstanceOf[RawHTML]), a)
    case BlockQuote(children)       => BlockQuote(children.map(b))
    case ListBlock(data, items)     => ListBlock(data, items.map(it => ListItem(it.content.map(b))))
    case other                      => other
  Document(doc.children.map(b))

val safe = renderToHTML(stripRawHtml(parseDocumentContent(input)), MarkdownConfig.default)
```

This is *not* a substitute for a real HTML sanitizer — emphasis text can still contain dangerous URLs in `Link` destinations, etc. But it's the right shape when your goal is "this AST is what I want, render it the normal way."

## Reusing the built-in inline renderer

If you want to keep the built-in inline rendering and only change block-level output, call `renderInlines` directly:

```scala
def myCustomBlock(b: Block): String = b match
  case Heading(level, inlines, _) => s"<my-h$level>${renderInlines(inlines)}</my-h$level>"
  case Paragraph(inlines)         => s"<my-p>${renderInlines(inlines)}</my-p>"
  case other                      => renderBlockToHTML(other)   // fall back
```

That gives you "HTML, but the wrappers I want" for free.

## Reaching into `processInlines`

If you build `Block` values *programmatically* — e.g. you constructed a `Heading(2, List(C(...)), None)` from a non-markdown source — and want to re-resolve the inline content, call `node.processInlines(linkRefs, config)`:

```scala
val raw  = Heading(2, List(/* C cursors */), None)
val done = raw.processInlines(Map.empty, MarkdownConfig.default)
```

This is rarely needed in practice. `parseDocumentContent` always returns a fully-resolved document.
