package io.github.edadma.markdown

/** Configuration for the Markdown processor
  */
case class MarkdownConfig(
    enableDefinitionLists: Boolean = false,
    // We can add more extension options here in the future
)

object MarkdownConfig {
  // Default configuration with standard CommonMark features only
  val default: MarkdownConfig = MarkdownConfig()

  // Convenience method for extensions
  def withExtensions(definitionLists: Boolean = false): MarkdownConfig =
    MarkdownConfig(enableDefinitionLists = definitionLists)
}
