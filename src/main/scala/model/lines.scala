package model

object lines {
  case class Directive(directive:String) extends ASTNode
  case class Widget(name:WName, widgetBody:List[ASTNode]) extends ASTNode
  case class Canvas(canvasBody:List[ASTNode]) extends ASTNode
  case class Instruction(name:WName, instructionBody:List[ASTNode]) extends ASTNode
  case class Property(name:String, propertyBody:List[Python]) extends ASTNode
  case class Comment(comment:String) extends ASTNode
  case object Blank extends ASTNode
  case class ClassRule(classWidget:AutoClass, widgetBody:List[ASTNode]) extends ASTNode
  case class ClassList(names:List[KivyString]) extends ASTNode
  case class AutoClass(widgetList:ClassList, widgetBase:Option[ClassList]) extends ASTNode
  case class Template(classWidget:AutoClass, widgetBody:List[ASTNode]) extends ASTNode

  sealed trait KivyString extends ASTNode
  case class WName(name:String) extends KivyString
  case class Reset(name:String) extends KivyString

  case class Python(pCode:String) extends ASTNode
}
