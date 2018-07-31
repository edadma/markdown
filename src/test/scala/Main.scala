package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |Here's a simple block:
      |
      |<div>
      |    foo
      |</div>
      |
      |This should be a code block, though:
      |
      |    <div>
      |        foo
      |    </div>
      |
      |As should this:
      |
      |    <div>foo</div>
      |
      |Now, nested:
      |
      |<div>
      |    <div>
      |        <div>
      |            foo
      |        </div>
      |    </div>
      |</div>
      |
      |This should just be an HTML comment:
      |
      |<!-- Comment -->
      |
      |Multiline:
      |
      |<!--
      |Blah
      |Blah
      |-->
      |
      |Code block:
      |
      |    <!-- Comment -->
      |
      |Just plain comment, with trailing spaces on the line:
      |
      |<!-- foo -->
      |
      |Code:
      |
      |    <hr />
      |
      |Hr's:
      |
      |<hr/>
      |
      |<hr />
      |
      |<hr/>
      |
      |<hr />
      |
      |<hr class="foo" id="bar" />
      |
      |<hr class="foo" id="bar"/>
    """.trim.stripMargin

  val doc = Markdown( input )

  Util.headingIds( doc )

  val html = Util.html( doc, 2 )

  println( html )

}
