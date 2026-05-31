package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FenceVsDefinitionListRegressionTest extends AnyFlatSpec with Matchers {

  "a fenced code block whose first line starts with ':'" should
    "render as code, not a definition list" in {
    val md =
      """```css
        |:root {
        |  --salle-radius: 0.25rem;
        |}
        |```
        |""".stripMargin

    val html = renderToHTML(md, MarkdownConfig().copy(definitionLists = true))

    html should include("<pre><code")
    html should not include "<dl>"
    html should not include "<dt>"
  }

  "a tilde-fenced code block whose first content line starts with ':'" should
    "render as code, not a definition list" in {
    val md =
      """~~~css
        |:root { color: red }
        |~~~
        |""".stripMargin

    val html = renderToHTML(md, MarkdownConfig().copy(definitionLists = true))

    html should include("<pre><code")
    html should not include "<dl>"
  }

  "a genuine definition list" should "still render with definition lists enabled" in {
    val md =
      """Term
        |: Definition
        |""".stripMargin

    val html = renderToHTML(md, MarkdownConfig().copy(definitionLists = true))

    html should include("<dl>")
    html should include("<dt>")
    html should include("<dd>")
  }
}
