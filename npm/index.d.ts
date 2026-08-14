/**
 * @edadma/markdown — TypeScript typings.
 *
 * The runtime is the linked Scala.js bundle at ./main.js. See README.md
 * for usage.
 */

/** All flags default to `false` unless otherwise noted. */
export interface MarkdownOptions {
  /** GFM-style pipe tables. */
  tables?: boolean;
  /** GFM `~~strikethrough~~`. */
  strikethrough?: boolean;
  /** GFM task list items (`- [ ]` / `- [x]`). */
  taskListItems?: boolean;
  /** GFM extended autolinks (bare URLs and emails turn into `<a>` tags). */
  extendedAutolinks?: boolean;
  /** `[^label]` references with `[^label]: …` definitions. */
  footnotes?: boolean;
  /** Curly quotes, en/em dashes, ellipsis. */
  smartPunctuation?: boolean;
  /** `$inline$` and `$$display$$` math (rendered as `<span>` / `<div>` for KaTeX/MathJax). */
  math?: boolean;
  /** `> [!NOTE]` admonition blocks. */
  callouts?: boolean;
  /** Definition lists (term + `: definition`). */
  definitionLists?: boolean;
  /** `{#id .class key=value}` attribute syntax on headings, fenced blocks, and images. */
  attributes?: boolean;
  /**
   * Auto-generate an `id` attribute on every heading from its text content
   * (slugified). Explicit ids set via the `attributes` extension always win.
   */
  autoHeadingIds?: boolean;
}

/** A flat heading entry returned by {@link extractHeadings}. */
export interface Heading {
  /** Heading level, 1–6. */
  level: number;
  /** Plain-text content of the heading (no markdown formatting). */
  text: string;
  /** Slugified id suitable for an HTML anchor (`<h2 id="…">`). */
  id: string;
}

/**
 * Render a markdown source string to HTML.
 *
 * @param md      the markdown source
 * @param options optional feature flags (see {@link MarkdownOptions})
 * @returns       the rendered HTML
 */
export function renderToHTML(md: string, options?: MarkdownOptions): string;

/**
 * Extract a flat list of headings from a markdown source. Auto-id generation
 * is implicitly enabled — every entry has an `id`.
 *
 * @param md      the markdown source
 * @param options optional feature flags (see {@link MarkdownOptions})
 * @returns       a flat list of {@link Heading} entries in source order
 */
export function extractHeadings(md: string, options?: MarkdownOptions): Heading[];

/**
 * Strip a markdown source down to plain text.
 *
 * Useful for previews, search-index excerpts, and tooltip text. Block
 * structure is collapsed: heading / paragraph / list-item content is
 * concatenated with single spaces between blocks.
 *
 * @param md      the markdown source
 * @param options optional feature flags (see {@link MarkdownOptions})
 * @returns       the plain-text projection
 */
export function plainText(md: string, options?: MarkdownOptions): string;

/** Library version, e.g. `"0.4.7"`. */
export const version: string;
