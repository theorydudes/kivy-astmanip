package model

object lines {
  trait LineNode extends ASTNode
  case class Directive(directive:String) extends LineNode
}
