package io.github.edadma.markdown

/** Configuration for the Markdown processor
  */
// Add the enableTables parameter
case class MarkdownConfig(
    enableDefinitionLists: Boolean = false,
    enableTables: Boolean = false,
    // We can add more extension options here in the future
)

object MarkdownConfig {
  // Default configuration with standard CommonMark features only
  val default: MarkdownConfig = MarkdownConfig()

  // Convenience method for extensions
  def withExtensions(
      definitionLists: Boolean = false,
      tables: Boolean = false,
  ): MarkdownConfig = MarkdownConfig(
    enableDefinitionLists = definitionLists,
    enableTables = tables,
  )
}
