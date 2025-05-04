package io.github.edadma.markdown

/** Configuration for the Markdown processor
  */
// Add the enableTables parameter
case class MarkdownConfig(
    enableDefinitionLists: Boolean = false,
    enableTables: Boolean = false,
    enableMath: Boolean = false,
)

object MarkdownConfig {
  val default: MarkdownConfig = MarkdownConfig()

  def withExtensions(
      definitionLists: Boolean = false,
      tables: Boolean = false,
      math: Boolean = false, // New parameter
  ): MarkdownConfig = MarkdownConfig(
    enableDefinitionLists = definitionLists,
    enableTables = tables,
    enableMath = math, // New setting
  )
}
