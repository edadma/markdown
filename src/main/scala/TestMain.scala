package ca.hyperreal.__markdown__


object TestMain extends App
{
    val s =
"""
<And nested without indentation:

<div>bar</div>

asdf
"""

    println( Markdown.asXML(s) )
}