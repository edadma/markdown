package ca.hyperreal.__markdown__


trait Document
case class Text( s: String ) extends Document
case class Emphasis( d: List[Document] ) extends Document
case class Strong( d: List[Document] ) extends Document
case class Group( d: List[Document] ) extends Document
case class Paragraph( d: List[Document] ) extends Document