---
title: Embedding the parser
summary: Drop the library into your Scala app — JVM, JS, or Native — and render markdown content with the right config for your audience.
weight: 10
---

The library has zero runtime third-party dependencies, and the same line in `build.sbt` works on every platform. Embedding it in an existing app is a one-import change.

## 1. Add the dependency

```scala
libraryDependencies += "io.github.edadma" %%% "markdown" % "0.4.6"
```

The `%%%` form picks the right artifact for whichever Scala platform you're on.

## 2. Pick a config that fits your audience

Three common shapes:

```scala
import io.github.edadma.markdown.*

// User comments, support tickets — predictable, no surprises.
val commentCfg = MarkdownConfig.default

// Knowledge base / wiki — table-heavy, link-heavy, anchor-heavy.
val wikiCfg = MarkdownConfig.default.copy(
  tables            = true,
  extendedAutolinks = true,
  autoHeadingIds    = true,
  smartPunctuation  = true,
)

// Author-facing docs / notes — full Obsidian / Hugo flavour.
val docsCfg = MarkdownConfig.all
```

There's no global config. Pass the one you want at every call site, or store one per-context.

## 3. Render

```scala
class CommentService:
  def renderComment(body: String): String =
    renderToHTML(body, commentCfg)
```

That's the full integration for the simple case.

## 4. Cache the parsed AST when it pays

For server-side rendering of frequently re-rendered content (a comment loaded a thousand times, a doc page hit on every request), cache the `Document` rather than the rendered HTML — it's smaller and the rendering step is cheap.

```scala
import io.github.edadma.markdown.*

class CachedRenderer(cfg: MarkdownConfig):
  private val cache = scala.collection.mutable.HashMap.empty[String, Document]

  def render(key: String, body: => String): String =
    val doc = cache.getOrElseUpdate(key, parseDocumentContent(body, cfg))
    renderToHTML(doc, cfg)
```

Invalidation strategy is your problem; the library is referentially transparent over `(input, config)`.

## 5. Preserve link references across documents

Cross-document analysis (a wiki where `[Page]` resolves against another file's link reference table) needs the references separately:

```scala
val (doc, refs) = parseDocumentContentWithRefs(body, cfg)
```

`refs: Map[String, LinkReference]` carries every `[label]: dest "title"` that appeared in the source. Hold them in your store; merge across files at render time as you see fit.

## 6. Sanitization

The library does **not** sanitize. `RawHTML` and `HTMLBlock` pass through verbatim — that's the CommonMark contract. If you're rendering untrusted input, run the output through a sanitizer (jsoup's `Cleaner`, OWASP HTML Sanitizer, DOMPurify on the JS side) **after** rendering.

You could also walk the AST and remove raw-HTML nodes before rendering — see [Custom renderer](/recipes/custom-renderer/) for the walker pattern.

## 7. Cross-platform notes

The same code runs on JVM, Scala.js (Node 20+), and Scala Native. The only platform-specific concern is filesystem I/O (`cross_platform.readFile` / `writeFile`), which is exposed by a sister library if you need it.

For the JS side specifically, the published `@edadma/markdown` npm bundle gives you the same API as direct Scala-from-Scala.js consumption — pick whichever surface fits your stack.
