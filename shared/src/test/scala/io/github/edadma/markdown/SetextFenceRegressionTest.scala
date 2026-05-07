package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for the precedence rule between
  * [[SetextHeadingBlockParser]] and [[FencedCodeBlockParser]].
  *
  * The setext parser is tried first in the block-parser registry (so it can
  * see "this paragraph line + the next line of `===` / `---`" patterns),
  * and used to claim any non-blank line whose next line was an underline.
  * That misread fenced code openers like
  *
  * {{{
  *   ```markdown
  *   ---
  *   title: Home
  *   ---
  *   ```
  * }}}
  *
  * as `<h2>\`\`\`markdown</h2>` followed by raw content. Per CommonMark
  * precedence, fenced code blocks win — a line that would open a fence
  * cannot be the first line of a setext heading. Fix in
  * [[SetextHeadingBlockParser.canStart]] (FenceOpenerPattern check).
  */
class SetextFenceRegressionTest extends AnyFlatSpec with Matchers {

  "setext heading vs fenced code precedence" should
    "treat ```fence content with YAML-style --- as code, not a setext h2" in {
    val md =
      """## Section 3
        |
        |```markdown
        |---
        |title: Home
        |---
        |
        |# Hello, world
        |```
        |
        |## Section 4
        |""".stripMargin

    val html = renderToHTML(md)

    html should include("<h2>Section 3</h2>")
    html should include("<h2>Section 4</h2>")
    html should include("<pre><code")
    // The fence content must NOT have been promoted to headings:
    html should not include "<h2>```markdown</h2>"
    html should not include "<h2>title: Home</h2>"
    html should not include "<h1>Hello, world</h1>"
  }

  it should "do the same for tilde fences" in {
    val md =
      """First section
        |
        |~~~
        |---
        |~~~
        |
        |Second section
        |""".stripMargin

    val html = renderToHTML(md)

    html should include("<pre><code")
    html should not include "<h2>~~~</h2>"
  }

  it should "still recognise an ordinary setext h2 (paragraph + ---)" in {
    val md =
      """A regular setext heading
        |---
        |
        |Body.
        |""".stripMargin

    val html = renderToHTML(md)

    html should include("<h2>A regular setext heading</h2>")
  }

  it should "still recognise an ordinary setext h1 (paragraph + ===)" in {
    val md =
      """A regular setext heading
        |===
        |
        |Body.
        |""".stripMargin

    val html = renderToHTML(md)

    html should include("<h1>A regular setext heading</h1>")
  }

  it should "treat a fence opened mid-paragraph correctly" in {
    // CommonMark: a fence can interrupt a paragraph.
    val md =
      """A paragraph
        |```code
        |inside
        |```
        |""".stripMargin

    val html = renderToHTML(md)

    html should include("<pre><code")
    // The paragraph text comes first, then the code block.
    html should include("A paragraph")
  }
}
