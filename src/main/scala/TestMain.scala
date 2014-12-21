package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
* a
 * b
"""
// *   Abacus
//     * answer
// *   Bubbles
//     1.  bunk
//     2.  bupkis
//         * BELITTLER
//     3. burper
// *   Cunning

	println( GFM(s) )
//	println( headings )
}