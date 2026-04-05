package io.github.edadma.markdown

/** Content-parsing mode for a doc-tag body. */
enum ContentMode:
  case Opaque         // body stored as a single Text node, no inline parsing
  case InlineMarkdown // body parsed with the inline parser
  case BlockMarkdown  // body parsed with the block parser

/** Registry entry defining one doc-tag.
  *
  * @param name
  *   the tag name (matched case-sensitively after the `@`)
  * @param acceptsTarget
  *   whether an identifier target is parsed after the name
  * @param contentMode
  *   how the body is parsed
  */
case class TagDefinition(
    name: String,
    acceptsTarget: Boolean,
    contentMode: ContentMode,
)

/** Lookup table for doc-tag definitions. */
final class TagRegistry(private val entries: Map[String, TagDefinition]):
  def lookup(name: String): Option[TagDefinition] = entries.get(name)
  def isEmpty: Boolean                             = entries.isEmpty
  def nonEmpty: Boolean                            = entries.nonEmpty
  def all: Iterable[TagDefinition]                 = entries.values

object TagRegistry:
  val empty: TagRegistry = new TagRegistry(Map.empty)

  def apply(defs: TagDefinition*): TagRegistry =
    new TagRegistry(defs.map(d => d.name -> d).toMap)

  def fromList(defs: Iterable[TagDefinition]): TagRegistry =
    new TagRegistry(defs.map(d => d.name -> d).toMap)

/** Configuration for the doc-tag extension.
  *
  * @param enabled
  *   master switch; when false, `@`-lines parse as ordinary text
  * @param registry
  *   tag definitions; empty is allowed
  * @param strictUnknownTags
  *   if true, unknown tags fall back to ordinary text parsing; if false (default), unknown tags are emitted as
  *   `DocTagBlock` with `acceptsTarget = true` and `InlineMarkdown` content mode
  */
case class DocTagConfig(
    enabled: Boolean = false,
    registry: TagRegistry = TagRegistry.empty,
    strictUnknownTags: Boolean = false,
)

object DocTagConfig:
  val disabled: DocTagConfig = DocTagConfig()
