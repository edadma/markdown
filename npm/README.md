# @edadma/markdown

A fast, cross-platform [CommonMark 0.31.2](https://spec.commonmark.org/0.31.2/)
markdown parser. **Full spec compliance** — all 652 spec tests pass on this
build.

This is the npm distribution of the Scala 3 / Scala.js library
[`io.github.edadma:markdown`](https://github.com/edadma/markdown). The npm
package ships the linked ES module so you can use it from any JavaScript /
TypeScript project without a Scala toolchain.

## Install

```bash
npm install @edadma/markdown
```

## Usage

```js
import { renderToHTML, extractHeadings, plainText, version } from "@edadma/markdown"

console.log(version)                                      // "0.4.3"

console.log(renderToHTML("# Hello, World!"))
// → "<h1>Hello, World!</h1>"

console.log(renderToHTML("# Hello, World!", { autoHeadingIds: true }))
// → '<h1 id="hello-world">Hello, World!</h1>'

console.log(extractHeadings("# Intro\n## Setup\n## Usage"))
// → [{ level: 1, text: "Intro", id: "intro" },
//    { level: 2, text: "Setup", id: "setup" },
//    { level: 2, text: "Usage", id: "usage" }]

console.log(plainText("# Title\n\nA *paragraph* with [a link](/x)"))
// → "Title A paragraph with a link"
```

## API

### `renderToHTML(md, options?)`

Render a markdown source string to HTML.

### `extractHeadings(md, options?)`

Returns a flat list of `{ level, text, id }` entries — one per top-level
heading in source order. Auto-id generation is implicitly enabled.

### `plainText(md, options?)`

Strip a markdown source down to plain text. Useful for previews, search-index
excerpts, and tooltip text. Headings, paragraphs, and list items are joined
with single spaces.

### Options

```ts
interface MarkdownOptions {
  tables?: boolean;             // GFM pipe tables
  strikethrough?: boolean;      // GFM ~~strikethrough~~
  taskListItems?: boolean;      // GFM `- [ ]` / `- [x]`
  extendedAutolinks?: boolean;  // GFM bare URLs and emails
  footnotes?: boolean;          // [^label] references
  smartPunctuation?: boolean;   // curly quotes, en/em dashes, ellipsis
  math?: boolean;               // $inline$ / $$display$$ math blocks
  callouts?: boolean;           // > [!NOTE] admonitions
  definitionLists?: boolean;    // term + ": definition"
  attributes?: boolean;         // {#id .class key=value} on headings/fences/images
  autoHeadingIds?: boolean;     // auto-generate <hN id="..."> from heading text
}
```

All flags default to `false` (pure CommonMark output).

Full type definitions are in `index.d.ts`.

## Notes

- **CommonMark 0.31.2 compliance.** All 652 spec tests pass. Each option
  above adds a non-CommonMark extension on top.
- **Heading IDs.** With `autoHeadingIds: true`, ids are slugified the same
  way Hugo / mkdocs / GitHub do: lowercase, non-alphanumeric runs collapsed
  to `-`, leading and trailing `-` stripped. Explicit ids from the
  `attributes` extension (`## Heading {#anchor}`) always win.
- **Bundle size.** The linked module is one ES module of around a megabyte
  (full CommonMark covers a lot of ground — entity tables, emoji shortcodes,
  etc.). For browser deployments, bundle through your existing pipeline
  (Vite, Rollup, esbuild, etc.) — tree-shaking will drop unused exports.

## License

[ISC](LICENSE) — same as the upstream Scala library.
