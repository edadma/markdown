package ca.hyperreal.markdown


object MarkdownTest extends App
{
// 	val s =
// """
// *   Abacus
//     * answer
// *   Bubbles
//     1.  bunk
//     2.  bupkis
//         * BELITTLER
//     3. burper
// *   Cunning
// """

// 	val s =
// """
// *   A list item.
// 
//     With multiple paragraphs.
// 
// *   Bar
// """

// 	val s =
// """
// Heading
// =======
// text
// """

// 	val s =
// """
// * Abacus
//  * one
// """

// 	val s =
// """
// Colons can be used to align columns.
// 
// | Tables        | Are           | Cool  |
// | ------------- |:-------------:| -----:|
// | col 3 is      | right-aligned | $1600 |
// | col 2 is      | centered      |   $12 |
// | zebra stripes | are neat      |    $1 |
// 
// The outer pipes (|) are optional, and you don't need to make the raw Markdown line up prettily. You can also use inline Markdown.
// 
// Markdown | Less | Pretty
// --- | --- | ---
// *Still* | `renders` | **nicely**
// 1 | 2 | 3
// """

	val s =
"""
*   Abacus
    * answer
*   Bubbles
    1.  bunk
    2.  bupkis
        * BELITTLER
    3. burper
*   Cunning
"""
	
	println( Markdown(s) )
//	println( headings )
}