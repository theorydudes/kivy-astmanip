package model

object lines {
  trait LineNode extends ASTNode
  case class DirectiveNode(directive:String) extends LineNode
  case class WidgetNode(name:WNameRuleNode,widgetBody:List[LineNode]) extends LineNode
  case class CanvasNode(canvasBody:List[LineNode]) extends LineNode
  case class InstructionNode(name:WNameRuleNode,instructionBody:List[LineNode]) extends LineNode
  case class PropertyNode(name:String,propertyBody:List[PythonNode]) extends LineNode
  case class CommentNode(comment:String) extends LineNode
  case object BlankNode extends LineNode
  case class ClassRuleNode(classWidget:AutoClassNode,widgetBody:List[LineNode]) extends LineNode
  case class ClassListNode(names:List[StringRuleNode]) extends LineNode
  case class AutoClassNode(widgetList:ClassListNode,widgetBase:Option[ClassListNode]) extends LineNode
  case class TemplateRuleNode(classWidget:AutoClassNode,widgetBody:List[LineNode]) extends LineNode

  sealed trait StringRuleNode extends LineNode
  case class WNameRuleNode(name:String) extends StringRuleNode
  case class ResetRuleNode(name:String) extends StringRuleNode

  case class PythonNode(pCode:String) extends LineNode
}
