package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

//  val linkRefs = Map("ref" -> LinkReference("image.jpg", None))
//  val input    = "Shortcut image: ![ref]"
//
//  pprintln(parseInlineContent(input, linkRefs))

//  pprintln(parseInlineContent("""__zxcv *asdf*__"""))

//  val input =
//    """
//      |# _asdf_""".stripMargin
//
//  println(renderToHTML(input))

  val input =
    """
      |# Testing Math Support in Markdown
      |
      |## Basic Inline Math
      |
      |Regular text with an inline equation $E = mc^2$ in the middle of a sentence.
      |
      |Variables like $x$, $y$, and $z$ should render properly.
      |
      |Fractions work inline too: $\frac{1}{2}$ is half.
      |
      |## Display Math
      |
      |The quadratic formula is given by:
      |
      |$$x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$
      |
      |Maxwell's equations in differential form are:
      |
      |$$\begin{align}
      |\nabla \times \vec{E} &= -\frac{\partial \vec{B}}{\partial t} \\
      |\nabla \times \vec{H} &= \vec{J} + \frac{\partial \vec{D}}{\partial t} \\
      |\nabla \cdot \vec{D} &= \rho \\
      |\nabla \cdot \vec{B} &= 0
      |\end{align}$$
      |
      |## Special Cases
      |
      |Currency symbol $20 should not be treated as math.
      |
      |But this is math: $2+2=4$.
      |
      |This has dollars at the end: $ax^2 + bx + c = 0$
      |
      |## Complex Examples
      |
      |Inline matrix $\begin{pmatrix} a & b \\ c & d \end{pmatrix}$ should work.
      |
      |Summation: $\sum_{i=1}^{n} i = \frac{n(n+1)}{2}$
      |
      |$$\int_{0}^{\infty} e^{-x^2} dx = \frac{\sqrt{\pi}}{2}$$
      |
      |## Mixed Content
      |
      |- List item with *italic* and math $\alpha + \beta$
      |- Another item with **bold** and math $\vec{F} = m\vec{a}$
      |
      |> Blockquote with math $P(A|B) = \frac{P(B|A)P(A)}{P(B)}$""".stripMargin
//  val input =
//    """
//      |$$
//      |E = mc^2
//      |$$
//      |""".stripMargin
  val (doc, refs) =
    parseDocumentContentWithRefs(input, MarkdownConfig.withExtensions(definitionLists = true, math = true))

//  val input =
//    """
//      |- a
//      |- """.stripMargin
//  val (doc, refs) = parseDocumentContentWithRefs(input)

//  pprintln(refs)
  pprintln(doc)
  println(renderToHTML(doc))

//  pprintln(extractHeaders(doc))

//  pprintln(parseDocumentContent("	This is indented with a tab."))
