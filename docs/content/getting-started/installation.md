---
title: Installation
summary: Add the library as an sbt dependency, an npm package, or check out the repo.
weight: 10
---

## Scala (sbt)

```scala
libraryDependencies += "io.github.edadma" %%% "markdown" % "0.4.6"
```

The `%%%` form picks the right artifact for whichever Scala platform you're on — JVM, Scala.js, or Scala Native — so the same line works in any cross-built project.

```scala
import io.github.edadma.markdown.*

val html = renderToHTML("# Hello, CommonMark!")
// → "<h1>Hello, CommonMark!</h1>\n"
```

The package object exposes everything via `io.github.edadma.markdown.*`. There's no other namespace to learn.

## JavaScript / TypeScript (npm)

The Scala.js build is also published as an npm package, with TypeScript typings:

```bash
npm install @edadma/markdown
```

```js
import { renderToHTML, extractHeadings, plainText } from "@edadma/markdown"

renderToHTML("# Hello", { autoHeadingIds: true })
// → '<h1 id="hello">Hello</h1>'

extractHeadings("# Intro\n## Setup")
// → [{ level: 1, text: "Intro", id: "intro" },
//    { level: 2, text: "Setup", id: "setup" }]
```

The npm bundle's exact API surface lives in [`npm/README.md`](https://github.com/edadma/markdown/blob/main/npm/README.md) inside the repo.

## From source

```bash
git clone https://github.com/edadma/markdown.git
cd markdown
sbt test                                              # all tests, all platforms
sbt 'project markdownJVM' test                        # JVM only
sbt 'project markdownJS' test                         # JS (Node 20+)
sbt 'project markdownNative' test                     # Scala Native
```

The aggregate `sbt test` runs the full 652-test CommonMark spec on every platform plus the unit tests.

## Cross-build matrix

| Target          | Version           | Runtime requirement |
|-----------------|-------------------|---------------------|
| Scala           | 3.8.3             | —                   |
| JVM             | —                 | Java 17 or newer    |
| Scala.js        | 1.21.0            | Node.js 20+         |
| Scala Native    | 0.5.11            | Clang               |
| CommonMark spec | 0.31.2            | —                   |

## Verifying the install

```scala
import io.github.edadma.markdown.*

@main def smoke(): Unit =
  println(renderToHTML("**hello**"))
  // <p><strong>hello</strong></p>
```

If you see the `<strong>` tag, you're done. Move on to the [Quickstart](/getting-started/quickstart/) for a tour of the API surface most callers actually use.
