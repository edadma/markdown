package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MathBlockParserTest extends AnyFlatSpec with Matchers {
  private val config = MarkdownConfig().copy(math = true)

  "The math block parser" should "parse a simple math block" in {
    val input =
      """
        |$$
        |E = mc^2
        |$$""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      MathBlock("E = mc^2"),
    ))
  }

  it should "parse a multi-line math block" in {
    val input =
      """
        |$$
        |f(x) = \int_{-\infty}^{\infty} \hat{f}(\xi) e^{2\pi i \xi x} d\xi
        |$$""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      MathBlock("f(x) = \\int_{-\\infty}^{\\infty} \\hat{f}(\\xi) e^{2\\pi i \\xi x} d\\xi"),
    ))
  }

  it should "parse a math block with complex expressions" in {
    val input =
      """
        |$$
        |\begin{align}
        |\nabla \times \vec{E} &= -\frac{\partial \vec{B}}{\partial t} \\
        |\nabla \times \vec{H} &= \vec{J} + \frac{\partial \vec{D}}{\partial t} \\
        |\nabla \cdot \vec{D} &= \rho \\
        |\nabla \cdot \vec{B} &= 0
        |\end{align}
        |$$""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      MathBlock(
        """\begin{align}
          |\nabla \times \vec{E} &= -\frac{\partial \vec{B}}{\partial t} \\
          |\nabla \times \vec{H} &= \vec{J} + \frac{\partial \vec{D}}{\partial t} \\
          |\nabla \cdot \vec{D} &= \rho \\
          |\nabla \cdot \vec{B} &= 0
          |\end{align}""".stripMargin,
      ),
    ))
  }

  it should "parse a math block with single-line syntax" in {
    val input    = "$$x = \\frac{-b \\pm \\sqrt{b^2 - 4ac}}{2a}$$"
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      MathBlock("x = \\frac{-b \\pm \\sqrt{b^2 - 4ac}}{2a}"),
    ))
  }

  it should "handle math blocks adjacent to other blocks" in {
    val input =
      """
        |Some text before.
        |
        |$$
        |E = mc^2
        |$$
        |
        |Some text after.""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Paragraph(List(Text("Some text before."))),
      MathBlock("E = mc^2"),
      Paragraph(List(Text("Some text after."))),
    ))
  }

  it should "ignore math blocks when the feature is disabled" in {
    val input =
      """
        |$$
        |E = mc^2
        |$$""".stripMargin

    // Use default config (math disabled)
    val document = parseDocumentContent(input, MarkdownConfig.default)

    // Should be parsed as a paragraph instead of math block
    document.children.head shouldBe a[Paragraph]
  }
}
