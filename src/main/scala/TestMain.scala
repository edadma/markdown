package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
5.  Can list markers be indented?  Can ordered list markers be right-aligned?

    ``` markdown
     8. item 1
     9. item 2
    10. item 2a
    ```

6.  Is this one list with a horizontal rule in its second item,
    or two lists separated by a horizontal rule?

    ``` markdown
    * a
    * * * * *
    * b
    ```
"""

	println( Markdown(s) )
//	println( headings )
}