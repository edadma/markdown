package ca.hyperreal.markdown


object MarkdownTest extends App
{
	val s =
"""
qwer

``` not-Scala
asdf
```
"""

	println( Markdown(s) )
//	println( headings )
}