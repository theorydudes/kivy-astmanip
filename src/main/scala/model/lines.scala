package model

object lines {
  trait LineNode extends ASTNode
  case class DirectiveNode(directive:String) extends LineNode
  case class WidgetNode(name:String,widgetBody:List[LineNode]) extends LineNode
  case class CanvasNode(canvasBody:List[LineNode]) extends LineNode
  case class InstructionNode(name:String,instructionBody:List[LineNode]) extends LineNode
  case class PropertyNode(name:String,propertyBody:List[PythonNode]) extends LineNode
  case class CommentNode(comment:String) extends LineNode
  case class PythonNode(pCode:String) extends LineNode
}
