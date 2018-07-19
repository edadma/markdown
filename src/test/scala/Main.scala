package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |asdf
      |====
      |
      |`asdf`
      |
      |```html
      |<p>asdf</p>
      |```
    """.trim.stripMargin

  println( Markdown(input) )

}