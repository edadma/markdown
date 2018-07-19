package xyz.hyperreal.markdown


trait AST {
  def elements: Seq[AST]
}

case class SeqAST( seq: Seq[AST] ) extends AST {
  val contents = null

  override def elements = seq
}

trait BranchAST extends AST {
  val contents: AST

  def elements =
    contents match {
      case SeqAST( seq ) => seq
      case _ => Seq( contents )
    }
}

trait LeafAST extends AST {
  def elements = Nil

  val text: String
}

case class ParagraphAST( contents: AST ) extends BranchAST

case class BlockquoteAST( contents: AST ) extends BranchAST

case class HeadingAST( level: Int, contents: AST, var id: Option[String] = None ) extends BranchAST

case class CodeInlineAST( text: String ) extends LeafAST

case class CodeBlockAST( text: String, highlighted: Option[String], caption: Option[String] ) extends LeafAST

case class TextAST( text: String ) extends LeafAST

case class LinkAST( address: String, title: Option[String], contents: AST ) extends BranchAST

case class ListItemAST( contents: AST ) extends BranchAST

case class UnorderedListAST( contents: AST ) extends BranchAST

case class OrderedListAST( contents: AST ) extends BranchAST

case class ImageAST( address: String, title: Option[String], text: String ) extends LeafAST

case class EmphasisAST( contents: AST ) extends BranchAST

case class StrongAST( contents: AST ) extends BranchAST

case class StrikethroughAST( contents: AST ) extends BranchAST

case object BreakAST extends LeafAST { val text = "\n" }

case object RuleAST extends LeafAST { val text = "" }

case class TableCell( align: String, contents: AST ) extends BranchAST

case class TableRow( contents: AST ) extends BranchAST

case class TableHead( contents: AST ) extends BranchAST

case class TableBody( contents: AST ) extends BranchAST

case class TableAST( contents: AST ) extends BranchAST

case class EntityAST( entity: String, text: String ) extends LeafAST
