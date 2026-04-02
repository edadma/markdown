package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.highlighter.*

class CodeHighlighterTest extends AnyFlatSpec with Matchers {

  val grammarJson = """{
    "scopeName": "source.test",
    "patterns": [
      { "match": "\\b(val|def|if|else|return)\\b", "name": "keyword.control.test" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.test" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.test" },
      { "match": "//.*$", "name": "comment.line.test" }
    ]
  }"""

  val Right(hl) = Highlighter.fromJson(grammarJson, ClassMode("hl-")): @unchecked

  val highlighter: (String, String) => Option[String] = (code, lang) =>
    if lang == "test" then Some(hl.highlight(code)) else None

  val config = MarkdownConfig(codeHighlighter = Some(highlighter))

  "Code highlighter integration" should "highlight fenced code blocks with a known language" in {
    val md = "```test\nval x = 42\n```"
    val html = renderToHTML(md, config)
    html should include("""class="hl-keyword"""")
    html should include("""class="hl-number"""")
    html should include("""class="language-test"""")
  }

  it should "fall back to plain rendering for unknown languages" in {
    val md = "```unknown\nval x = 42\n```"
    val html = renderToHTML(md, config)
    html should include("""class="language-unknown"""")
    html should not include "hl-"
    html should include("val x = 42")
  }

  it should "fall back to plain rendering when no language specified" in {
    val md = "```\nval x = 42\n```"
    val html = renderToHTML(md, config)
    html should not include "hl-"
    html should include("val x = 42")
  }

  it should "not highlight when no codeHighlighter configured" in {
    val md = "```test\nval x = 42\n```"
    val html = renderToHTML(md)
    html should not include "hl-"
    html should include("val x = 42")
  }

  it should "work with inline style mode" in {
    val Right(inlineHl) = Highlighter.fromJson(grammarJson, InlineMode(Theme.OneDark)): @unchecked
    val inlineHighlighter: (String, String) => Option[String] = (code, lang) =>
      if lang == "test" then Some(inlineHl.highlight(code)) else None

    val inlineConfig = MarkdownConfig(codeHighlighter = Some(inlineHighlighter))
    val md = "```test\nval x = 42\n```"
    val html = renderToHTML(md, inlineConfig)
    html should include("style=\"color:")
  }
}
