package xyz.hyperreal.markdown

import java.io._


object TestMain extends App {

  println( prettyPrint(Markdown.withHeadings(
    """
      |1.
      |==
      |
      |1.2.
      |----
      |
      |2.
      |==
      |
      |3.
      |==
      |
      |3.1.
      |----
      |
      |3.2.
      |----
      |
      |### 3.2.1.
      |
      |4.
      |==
    """.stripMargin
  )._2) )

  def prettyPrint(a: Any, indentSize: Int = 2, maxElementWidth: Int = 30, depth: Int = 0): String = {
    val indent = " " * depth * indentSize
    val fieldIndent = indent + (" " * indentSize)
    val thisDepth = prettyPrint(_: Any, indentSize, maxElementWidth, depth)
    val nextDepth = prettyPrint(_: Any, indentSize, maxElementWidth, depth + 1)
    a match {
      // Make Strings look similar to their literal form.
      case s: CharSequence =>
        val replaceMap = Seq(
          "\\" -> "\\\\",
          "\n" -> "\\n",
          "\r" -> "\\r",
          "\t" -> "\\t",
          "\"" -> "\\\""
        )
        '"' + replaceMap.foldLeft(s.toString) { case (acc, (c, r)) => acc.replace(c, r) } + '"'
      // For an empty Seq just use its normal String representation.
      case m: collection.Map[_, _] if m isEmpty => "Map()"
      case m: collection.Map[_, _] =>
        m.map { case (k, v) => s"\n$fieldIndent${nextDepth(k)} -> ${nextDepth(v)}" }.mkString( "Map(", ", ", s"\n$indent)" )
      case xs: Seq[_] if xs.isEmpty => "Nil"
      case xs: Seq[_] =>
        // If the Seq is not too long, pretty print on one line.
        val resultOneLine = xs.map(nextDepth).toString()
        if (resultOneLine.length <= maxElementWidth) return resultOneLine
        // Otherwise, build it with newlines and proper field indents.
        val result = xs.map(x => s"\n$fieldIndent${nextDepth(x)}").toString()
        result.substring(0, result.length - 1) + "\n" + indent + ")"
      // Product should cover case classes.
      case p: Product =>
        val prefix = p.productPrefix
        // We'll use reflection to get the constructor arg names and values.
        //        val cls = p.getClass
        //        val fields = cls.getDeclaredFields.filterNot(_.isSynthetic).map(_.getName)
        val fields = scala.reflect.runtime.currentMirror.reflect(p).symbol.typeSignature.decls.
          filter(s => s.isMethod && s.asMethod.isCaseAccessor).toList.map(_.name)
        val values = p.productIterator.toSeq
        if (fields.length != values.length) sys.error( s"fields and values lists have unequal length: $p" )
        fields.zip(values) match {
          // If there are no fields, just use the normal String representation.
          case Nil => p.toString
          // If there is just one field, let's just print it as a wrapper.
          case (_, value) :: Nil => s"$prefix(${thisDepth(value)})"
          // If there is more than one field, build up the field names and values.
          case kvps =>
            val prettyFields = kvps.map { case (k, v) => s"$fieldIndent$k = ${nextDepth(v)}" }
            // If the result is not too long, pretty print on one line.
            val resultOneLine = s"$prefix(${prettyFields.mkString(", ")})"
            if (resultOneLine.length <= maxElementWidth) return resultOneLine
            // Otherwise, build it with newlines and proper field indents.
            s"$prefix(\n${prettyFields.mkString(",\n")}\n$indent)"
        }
      // If we haven't specialized this type, just use its toString.
      case _ => a.toString
    }
  }

}