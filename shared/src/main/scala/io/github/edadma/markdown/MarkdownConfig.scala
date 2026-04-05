package io.github.edadma.markdown

/** Configuration for the Markdown processor
  */
// Add the enableTables parameter
case class MarkdownConfig(
    definitionLists: Boolean = false,
    tables: Boolean = false,
    math: Boolean = false,
    emoji: EmojiConfig = EmojiConfig.Disabled,
    callouts: Boolean = false,
    strikethrough: Boolean = false,
    taskListItems: Boolean = false,
    extendedAutolinks: Boolean = false,
    footnotes: Boolean = false,
    smartPunctuation: Boolean = false,
    attributes: Boolean = false,
    docTags: DocTagConfig = DocTagConfig.disabled,
    codeHighlighter: Option[(String, String) => Option[String]] = None,
    indentedCodeLanguage: Option[String] = None,
    indentedCodeBreaksList: Boolean = false,
)

object MarkdownConfig:
  val default: MarkdownConfig = MarkdownConfig()
  val all: MarkdownConfig =
    MarkdownConfig(definitionLists = true, tables = true, math = true, emoji = EmojiConfig.Unicode, callouts = true,
      strikethrough = true, taskListItems = true, extendedAutolinks = true, footnotes = true,
      smartPunctuation = true, attributes = true)

enum EmojiConfig:
  case Disabled
  case Unicode
  case Image(baseURL: String)
