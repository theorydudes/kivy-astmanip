package model

object nodes {
  trait WidgetBodyElement extends ASTNode
  trait InstructionBodyElement extends ASTNode
  trait CanvasBodyElement extends ASTNode

  type WidgetBody = List[WidgetBodyElement]
  type InstructionBody = List[InstructionBodyElement]
  type CanvasBody = List[CanvasBodyElement]

  case class Directive(directive:String) extends ASTNode
  case class Widget(name:WName, widgetBody:WidgetBody) extends WidgetBodyElement
  case class Canvas(canvasBody:CanvasBody) extends WidgetBodyElement
  case class Instruction(name:WName, instructionBody:InstructionBody) extends CanvasBodyElement
  case class Property(name:String, propertyBody:List[Python]) extends WidgetBodyElement with InstructionBodyElement
  case class Comment(comment:String) extends WidgetBodyElement with InstructionBodyElement with CanvasBodyElement
  case class ClassRule(classWidget:AutoClass, widgetBody:List[ASTNode]) extends ASTNode
  case class ClassList(names:List[KivyString]) extends ASTNode
  case class AutoClass(widgetList:ClassList, widgetBase:Option[ClassList]) extends ASTNode
  case class Template(classWidget:AutoClass, widgetBody:List[ASTNode]) extends ASTNode

  sealed trait KivyString extends ASTNode
  case class WName(name:String) extends KivyString {
    override def toString: String = name
  }
  case class Reset(name:String) extends KivyString {
    override def toString: String = s"-$name"
  }

  case class Python(pCode:String) extends ASTNode {
    /**
     * @return textual representation of python Code contained in `pCode` ,
     *         but wihtout leading and ending WhiteSpace characters.
     */
    override def toString: String = {
      val firstIndexWithoutWhiteSpaces =
        pCode
        .indexWhere(c => """[^\s]""".r.pattern.matcher(c.toString).matches())

      val lastIndexBeforeWhieSpacesBegin =
        pCode
          .lastIndexWhere(c => """[^\s]""".r.pattern.matcher(c.toString).matches()) + 1

      pCode.substring(
        if(firstIndexWithoutWhiteSpaces != -1) firstIndexWithoutWhiteSpaces else 0,
        if(lastIndexBeforeWhieSpacesBegin != -1) lastIndexBeforeWhieSpacesBegin else pCode.lastIndexOf(pCode.last + 1)
      )
    }
  }
}
