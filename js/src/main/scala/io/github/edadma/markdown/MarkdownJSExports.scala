package io.github.edadma.markdown

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._

/** ESM exports for npm consumers.
  *
  * Compiled into the linked `main.js` via `scalaJSLinkerConfig.withModuleKind(ESModule)`
  * (set in `build.sbt`). The TypeScript surface is documented in `npm/index.d.ts`;
  * keep the two in sync.
  *
  * Each function takes a JS-friendly options dictionary rather than a Scala
  * `MarkdownConfig` so callers don't have to know about the case-class shape.
  * Unrecognised keys are silently ignored.
  */
object MarkdownJSExports {

  // ---------- options-object → MarkdownConfig ----------

  private def boolFlag(opts: js.UndefOr[js.Dictionary[Any]], key: String, default: Boolean): Boolean =
    opts.toOption
      .flatMap(_.get(key))
      .collect { case b: Boolean => b }
      .getOrElse(default)

  private def parseConfig(opts: js.UndefOr[js.Dictionary[Any]]): MarkdownConfig =
    MarkdownConfig.default.copy(
      tables             = boolFlag(opts, "tables", false),
      strikethrough      = boolFlag(opts, "strikethrough", false),
      taskListItems      = boolFlag(opts, "taskListItems", false),
      extendedAutolinks  = boolFlag(opts, "extendedAutolinks", false),
      footnotes          = boolFlag(opts, "footnotes", false),
      smartPunctuation   = boolFlag(opts, "smartPunctuation", false),
      math               = boolFlag(opts, "math", false),
      callouts           = boolFlag(opts, "callouts", false),
      definitionLists    = boolFlag(opts, "definitionLists", false),
      attributes         = boolFlag(opts, "attributes", false),
      autoHeadingIds     = boolFlag(opts, "autoHeadingIds", false),
    )

  // ---------- exported entry points ----------

  /** Render a markdown source string to HTML. The optional `options` object
    * mirrors the `MarkdownConfig` boolean flags; see `index.d.ts` for the
    * full list.
    */
  @JSExportTopLevel("renderToHTML")
  def renderToHTMLJS(md: String, options: js.UndefOr[js.Dictionary[Any]] = js.undefined): String =
    renderToHTML(md, parseConfig(options))

  /** Extract a flat list of headings from a markdown source. Each entry
    * has the heading's level (1–6), its plain-text content, and its
    * auto-generated id (always present — `autoHeadingIds` is implicitly
    * enabled for this function).
    *
    * Returns a JS array of `{ level, text, id }` objects.
    */
  @JSExportTopLevel("extractHeadings")
  def extractHeadingsJS(md: String, options: js.UndefOr[js.Dictionary[Any]] = js.undefined): js.Array[js.Dynamic] = {
    val cfg = parseConfig(options).copy(autoHeadingIds = true)
    val doc = parseDocumentContent(md, cfg)
    doc.headings.map { h =>
      js.Dynamic.literal(
        level = h.level,
        text  = plainText(h.inlines),
        id    = h.attrs.flatMap(_.id).getOrElse(""),
      )
    }.toJSArray
  }

  /** Strip a markdown source down to plain text — useful for previews,
    * search-index excerpts, etc. Headings, list items, paragraphs, etc.
    * are concatenated with single spaces between blocks.
    */
  @JSExportTopLevel("plainText")
  def plainTextJS(md: String, options: js.UndefOr[js.Dictionary[Any]] = js.undefined): String = {
    val doc = parseDocumentContent(md, parseConfig(options))
    val buf = new StringBuilder
    def go(b: Block): Unit = b match {
      case p: Paragraph     => if (buf.nonEmpty) buf += ' '; buf ++= plainText(p.inlines)
      case h: Heading       => if (buf.nonEmpty) buf += ' '; buf ++= plainText(h.inlines)
      case BlockQuote(cs)   => cs.foreach(go)
      case ListBlock(_, items) =>
        items.foreach { case ListItem(children) => children.foreach(go) }
      case _ => ()
    }
    doc.children.foreach(go)
    buf.toString.trim
  }

  /** Library version. Useful for npm consumers that want to display "powered
    * by markdown 0.x.y" or similar. Comes from `ThisBuild / version` by way of
    * the generated `BuildVersion`, so it cannot drift from what was published.
    */
  @JSExportTopLevel("version")
  val versionJS: String = BuildVersion.value
}
