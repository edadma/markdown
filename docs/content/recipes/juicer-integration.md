---
title: Juicer integration
summary: Juicer — the static site generator powering this very site — uses this library to render content. Here's how the pieces fit.
weight: 30
---

[Juicer](https://juicer.run/) is a Hugo-style static site generator written in Scala 3. It uses this library as its sole markdown engine — meaning every `.md` file under your site's `content/` tree is parsed and rendered by exactly the code documented here.

## Where it sits

Juicer's pipeline is roughly:

```
content/*.md → frontmatter parse → parseDocumentContent (this library) → template render → HTML files
```

The `Document` AST is exposed to templates as `.page.content` (rendered HTML) and via individual heading / TOC accessors. Juicer also walks the AST to:

- build per-page tables of contents from `Document.headings`;
- generate stable URL slugs via `autoHeadingIds` + the configured `slugify`;
- produce the search index (`search.json`) from `plainText(...)` of each page;
- detect link reference targets across the content tree.

## Configuration

Juicer's site-level config exposes the relevant `MarkdownConfig` flags directly. From a `site.toml`:

```toml
[markdown]
tables             = true
strikethrough      = true
taskListItems      = true
extendedAutolinks  = true
smartPunctuation   = true
autoHeadingIds     = true
```

Those keys map 1-to-1 onto fields of `MarkdownConfig`. Flags not listed default to `false` (pure CommonMark). See [Options](/reference/options/) for the full set.

## Code highlighting

Juicer wires `codeHighlighter` to its bundled [highlighter](https://github.com/edadma/highlighter) library. Authoring a fenced block with a known language tag picks up syntax highlighting automatically; with no tag (or an unknown one), you get plain `<pre><code>`.

## Shortcodes

Juicer adds an extra layer *above* this library — its `\[= shortcode =]…\[= /shortcode =]` syntax is processed before markdown parsing. The result of expanding a shortcode is markdown text that this library then parses normally. So shortcodes don't change the spec compliance of the underlying parse.

## Cross-linking

Juicer treats unresolved markdown links as the SSG's job, not the parser's. A `[Page](page-slug)` in source goes through this library as a normal `Link(destination = "page-slug", ...)`; juicer rewrites the destination at render time to whatever URL the slug resolves to in the content tree. From this library's point of view, link resolution is purely string-level.

## A minimal juicer-like loop

The skeleton of "render every `.md` in `content/` to `public/`" is just:

```scala
import io.github.edadma.markdown.*
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

val cfg = MarkdownConfig.default.copy(
  tables = true, autoHeadingIds = true, smartPunctuation = true,
)

Files.walk(Paths.get("content")).iterator.asScala
  .filter(p => p.toString.endsWith(".md"))
  .foreach { src =>
    val md   = Files.readString(src)
    val html = renderToHTML(md, cfg)
    val out  = Paths.get("public", src.getFileName.toString.replace(".md", ".html"))
    Files.createDirectories(out.getParent)
    Files.writeString(out, html)
  }
```

Real SSGs add frontmatter parsing, templating, asset copying, and a full link-resolution pass — but the `parseDocumentContent` / `renderToHTML` pair is the unchanged core of the markdown step. The rest is plumbing.

## Further reading

- [Juicer — concepts](https://juicer.run/concepts/) — content-files, layouts, partials, themes.
- [Juicer — markdown](https://juicer.run/concepts/markdown/) — juicer's view of how content is parsed.
