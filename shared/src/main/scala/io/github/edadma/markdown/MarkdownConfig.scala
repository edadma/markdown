package io.github.edadma.markdown

/** Configuration for the Markdown processor
  */
// Add the enableTables parameter
case class MarkdownConfig(
    definitionListsEnabled: Boolean = false,
    tablesEnabled: Boolean = false,
    mathEnabled: Boolean = false,
)

object MarkdownConfig {
  val default: MarkdownConfig = MarkdownConfig()

  def withExtensions(
      definitionLists: Boolean = false,
      tables: Boolean = false,
      math: Boolean = false, // New parameter
  ): MarkdownConfig = MarkdownConfig(
    definitionListsEnabled = definitionLists,
    tablesEnabled = tables,
    mathEnabled = math, // New setting
  )
}
