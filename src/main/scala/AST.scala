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
}

trait BlockAST extends AST

trait InlineAST extends AST

case class ParagraphAST( contents: AST ) extends BlockAST with BranchAST

case class BlockquoteAST( contents: AST ) extends BlockAST with BranchAST

case class HeadingAST( level: Int, contents: AST, var id: Option[String] = None ) extends BlockAST with BranchAST

case class CodeInlineAST( text: String ) extends InlineAST with LeafAST

case class CodeBlockAST( text: String, highlighted: Option[String], caption: Option[String] ) extends BlockAST with LeafAST

case class TextAST( text: String ) extends InlineAST with LeafAST

case class RawAST( text: String ) extends InlineAST with LeafAST

case class LinkAST( address: String, title: Option[String], contents: AST ) extends InlineAST with BranchAST

case class ListItemAST( contents: AST ) extends InlineAST with BranchAST

case class UnorderedListAST(list: Seq[ListItemAST] ) extends BlockAST with LeafAST

case class OrderedListAST( list: Seq[ListItemAST] ) extends BlockAST with LeafAST

case class ImageAST( address: String, title: Option[String], alt: String ) extends InlineAST with LeafAST

case class EmphasisAST( contents: AST ) extends InlineAST with BranchAST

case class StrongAST( contents: AST ) extends InlineAST with BranchAST

case class StrikethroughAST( contents: AST ) extends InlineAST with BranchAST

case object BreakAST extends InlineAST with LeafAST

case object RuleAST extends InlineAST with LeafAST

case class TableCell( align: String, item: AST )

case class TableAST( head: Seq[Seq[TableCell]], body: Seq[Seq[TableCell]] ) extends BlockAST with LeafAST

case class EntityAST( entity: String, text: String ) extends InlineAST with LeafAST
