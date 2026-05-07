package io.github.edadma.markdown

/** Configuration for the Markdown processor
  */
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
    /** When `true`, every heading gets an auto-generated `id` attribute on its
      * rendered `<hN>` tag. The id is derived from the heading's plain-text
      * content via [[slugify]]. Explicit ids set by the `attributes` extension
      * (`## Heading {#anchor}`) always win.
      *
      * Off by default to keep pure-CommonMark output unchanged. SSGs and docs
      * tools typically want to turn this on.
      */
    autoHeadingIds: Boolean = false,
    /** Pluggable slug function used by [[autoHeadingIds]]. Default behaviour:
      * lowercase, replace non-alphanumeric runs with `-`, strip leading and
      * trailing `-`. Same algorithm Hugo / mkdocs / GitHub use for permalinks.
      */
    slugify: String => String = MarkdownConfig.defaultSlugify,
    docTags: DocTagConfig = DocTagConfig.disabled,
    codeHighlighter: Option[(String, String) => Option[String]] = None,
    indentedCodeLanguage: Option[String] = None,
    indentedCodeBreaksList: Boolean = false,
)

object MarkdownConfig:
  val default: MarkdownConfig = MarkdownConfig()
  val all: MarkdownConfig =
    MarkdownConfig(
      definitionLists = true,
      tables = true,
      math = true,
      emoji = EmojiConfig.Unicode,
      callouts = true,
      strikethrough = true,
      taskListItems = true,
      extendedAutolinks = true,
      footnotes = true,
      smartPunctuation = true,
      attributes = true,
      autoHeadingIds = true,
    )

  /** Default slug function for [[MarkdownConfig.autoHeadingIds]].
    *
    * Algorithm: lowercase, collapse runs of non-letter / non-digit characters
    * into a single `-`, strip leading and trailing `-`. Returns an empty
    * string if the input has no letter / digit characters at all.
    */
  def defaultSlugify(text: String): String = {
    val sb     = new StringBuilder
    var dash   = false
    val lower  = text.toLowerCase
    var i      = 0
    while (i < lower.length) {
      val ch = lower.charAt(i)
      if (ch.isLetterOrDigit) {
        sb += ch
        dash = false
      } else if (!dash && sb.nonEmpty) {
        sb += '-'
        dash = true
      }
      i += 1
    }
    val s = sb.toString
    if (s.endsWith("-")) s.dropRight(1) else s
  }

enum EmojiConfig:
  case Disabled
  case Unicode
  case Image(baseURL: String)
