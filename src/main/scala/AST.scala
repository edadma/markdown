package xyz.hyperreal.markdown


trait AST

case class SeqAST( seq: Seq[ElementAST] ) extends ElementAST {
  val contents = null

  override def elements = seq
}

trait ElementAST extends AST {
  def elements: Seq[ElementAST]
}

trait BranchElementAST extends ElementAST {
  val contents: ElementAST

  def elements =
    contents match {
      case SeqAST( seq ) => seq
      case _ => Seq( contents )
    }
}

trait LeafElementAST extends ElementAST {
  def elements = Nil
}

trait BlockElementAST extends ElementAST

case class ParagraphAST( contents: SeqAST ) extends BlockElementAST with BranchElementAST

trait InlineElementAST extends ElementAST

case class HeadingAST( level: Int, contents: ElementAST ) extends BlockElementAST with BranchElementAST

case class CodeInlineAST( text: String ) extends InlineElementAST with LeafElementAST

case class CodeBlockAST( text: String, highlighted: Option[String], caption: Option[String] ) extends BlockElementAST with LeafElementAST

case class TextAST( text: String ) extends InlineElementAST with LeafElementAST

case class RawAST( text: String ) extends InlineElementAST with LeafElementAST

case class LinkAST( address: String, title: Option[String], contents: ElementAST ) extends InlineElementAST with BranchElementAST

case class EmphasisAST( contents: ElementAST ) extends InlineElementAST with BranchElementAST

case class StrongAST( contents: ElementAST ) extends InlineElementAST with BranchElementAST

case object BreakAST extends InlineElementAST with LeafElementAST