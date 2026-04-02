package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.highlighter.*

class CodeHighlighterTest extends AnyFlatSpec with Matchers {

  // -- Setup: demonstrates the recommended integration pattern --
  // Parse each grammar once and cache the Highlighter instance per language.
  // The codeHighlighter function just does a map lookup — no re-parsing.

  val scalaGrammarJson = """{
    "scopeName": "source.scala",
    "patterns": [
      { "match": "\\b(val|var|def|class|object|if|else|match|case|for|while|return)\\b", "name": "keyword.control.scala" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.scala" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.scala" },
      { "match": "//.*$", "name": "comment.line.scala" }
    ]
  }"""

  val jsGrammarJson = """{
    "scopeName": "source.js",
    "patterns": [
      { "match": "\\b(const|let|var|function|return|if|else)\\b", "name": "keyword.control.js" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.js" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.js" },
      { "match": "//.*$", "name": "comment.line.js" }
    ]
  }"""

  val mode = ClassMode("hl-")

  // Cache: parse grammars once, reuse across all code blocks
  val highlighters: Map[String, Highlighter] = Map(
    "scala"      -> Highlighter.fromJson(scalaGrammarJson, mode).toOption.get,
    "javascript" -> Highlighter.fromJson(jsGrammarJson, mode).toOption.get,
    "js"         -> Highlighter.fromJson(jsGrammarJson, mode).toOption.get,
  )

  // The function passed to MarkdownConfig — just a map lookup per block
  val codeHighlighter: (String, String) => Option[String] = (code, lang) =>
    highlighters.get(lang).map(_.highlight(code))

  val config = MarkdownConfig(codeHighlighter = Some(codeHighlighter))

  // -- Tests --

  "Code highlighter integration" should "highlight fenced code blocks with a known language" in {
    val md = "```scala\nval x = 42\n```"
    val html = renderToHTML(md, config)
    html should include("""class="hl-keyword"""")
    html should include("""class="hl-number"""")
    html should include("""class="language-scala"""")
  }

  it should "highlight multiple languages in the same document" in {
    val md =
      """```scala
        |val x = 42
        |```
        |
        |```js
        |const y = 99
        |```""".stripMargin
    val html = renderToHTML(md, config)
    html should include("""class="language-scala"""")
    html should include("""class="language-js"""")
    // Both blocks should be highlighted
    html.split("hl-keyword").length should be >= 3
  }

  it should "fall back to plain rendering for unknown languages" in {
    val md = "```python\nprint(42)\n```"
    val html = renderToHTML(md, config)
    html should include("""class="language-python"""")
    html should not include "hl-"
    html should include("print(42)")
  }

  it should "fall back to plain rendering when no language specified" in {
    val md = "```\nval x = 42\n```"
    val html = renderToHTML(md, config)
    html should not include "hl-"
    html should include("val x = 42")
  }

  it should "not highlight when no codeHighlighter configured" in {
    val md = "```scala\nval x = 42\n```"
    val html = renderToHTML(md)
    html should not include "hl-"
    html should include("val x = 42")
  }

  it should "work with inline style mode" in {
    val inlineHighlighters = Map(
      "scala" -> Highlighter.fromJson(scalaGrammarJson, InlineMode(Theme.OneDark)).toOption.get,
    )
    val inlineConfig = MarkdownConfig(codeHighlighter =
      Some((code, lang) => inlineHighlighters.get(lang).map(_.highlight(code)))
    )
    val md = "```scala\nval x = 42\n```"
    val html = renderToHTML(md, inlineConfig)
    html should include("style=\"color:")
  }
}
