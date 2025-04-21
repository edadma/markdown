package io.github.edadma.markdown

def parse(cursors: LazyList[Cursor]): List[Inline] = {
  // Initially, just create a single Text node with all content
  List(Text(cursors.map(_.char).mkString))

  // Later, this will be replaced with actual inline parsing logic
  // that handles emphasis, links, code spans, etc.
}
