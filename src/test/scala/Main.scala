package xyz.hyperreal.markdown

import java.io._


object Main extends App {

  println( Markdown(
    """
      |```html "caption"
      |<p>this is a test</p>
      |```
    """.trim.stripMargin
  ) )

}