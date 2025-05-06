package io.github.edadma.markdown

/** Configuration for the Markdown processor
  */
// Add the enableTables parameter
case class MarkdownConfig(
    definitionLists: Boolean = false,
    tables: Boolean = false,
    math: Boolean = false,
    emoji: EmojiConfig = EmojiConfig.Disabled,
)

object MarkdownConfig:
  val default: MarkdownConfig = MarkdownConfig()
  val all: MarkdownConfig =
    MarkdownConfig(definitionLists = true, tables = true, math = true, emoji = EmojiConfig.Unicode)

enum EmojiConfig:
  case Disabled
  case Unicode
  case Image(baseURL: String)
