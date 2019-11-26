package model

import com.github.python3parser.model.mods.ExpressionMod

object lines {
  trait WidgetBodyElement extends ASTNode
  trait InstructionBodyElement extends ASTNode
  trait CanvasBodyElement extends ASTNode
  trait RootNodeElement extends ASTNode

  object CanvasType {
    trait canvasType
    case object Regular extends canvasType {
      override def toString: String = "canvas"
    }
    case object Before extends canvasType {
      override def toString: String = "canvas.before"
    }
    case object After extends canvasType {
      override def toString: String = "canvas.after"
    }
  }

  type RootLevelNodes = List[RootNodeElement]
  type WidgetBody = List[WidgetBodyElement]
  type InstructionBody = List[InstructionBodyElement]
  type CanvasBody = List[CanvasBodyElement]

  def foldList[B](ans:List[ASTNode])(z:B)(op:(B,ASTNode) => B): B =
    ans.foldLeft(z){(acc,an) => an.foldLeft(acc)(op)}

  def foldOption[B](opt:Option[ASTNode])(z:B)(op:(B,ASTNode) => B): B =
    opt.foldLeft(z){ (acc,an) => an.foldLeft(acc)(op)}

  case class TopLevel(rootLevelNodes: RootLevelNodes)
    extends ASTNode { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = {
      op(foldList(rootLevelNodes)(z)(op),self)
    }
  }

  case class Directive(directive:String)
    extends RootNodeElement {self =>

    override def toString: String = s"#:$directive"

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = op(z,self)
  }

  case class Root(widget:Widget)
    extends RootNodeElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = {
      op(widget.foldLeft(z)(op),self)
    }
  }
  case class Widget(name:WName, widgetBody:WidgetBody)
    extends WidgetBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = {
      val foldedName = name.foldLeft(z)(op)
      val foldedBody =  foldList(widgetBody)(foldedName)(op)
      op(foldedBody,self)
    }
  }
  case class Canvas(canvasType: CanvasType.canvasType,canvasBody:CanvasBody)
    extends WidgetBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = {
      val foldedBody = foldList(canvasBody)(z)(op)
      op(foldedBody,self)
    }
  }

  case class Instruction(name:WName, instructionBody:InstructionBody)
    extends CanvasBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(instructionBody)(name.foldLeft(z)(op))(op),self)
  }

  case class Property(name:String, propertyBody:List[Python])
    extends WidgetBodyElement with InstructionBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(propertyBody)(z)(op),self)
  }

  case class Comment(comment:String)
    extends WidgetBodyElement with InstructionBodyElement with CanvasBodyElement with RootNodeElement { self =>

    override def toString: String = s"#$comment"

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)
  }

  case class ClassRule(classWidget:AutoClass, widgetBody:List[ASTNode])
    extends RootNodeElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(widgetBody)(classWidget.foldLeft(z)(op))(op),self)
  }

  case class ClassList(names:List[KivyString])
    extends ASTNode { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(names)(z)(op),self)
  }

  case class ClassListBase(names:List[KivyString])
    extends ASTNode { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(names)(z)(op),self)
  }

  case class AutoClass(widgetList:ClassList, widgetBase:Option[ClassListBase])
    extends ASTNode { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldOption(widgetBase)(widgetList.foldLeft(z)(op))(op),self)
  }

  case class Template(classWidget:AutoClass, widgetBody:List[ASTNode])
    extends RootNodeElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(widgetBody)(classWidget.foldLeft(z)(op))(op),self)
  }

  sealed trait KivyString extends ASTNode

  case class WName(name:String)
    extends KivyString { self =>
    override def toString: String = name

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)
  }

  case class Reset(name:String)
    extends KivyString { self =>

    override def toString: String = s"-$name"

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)
  }

  case class Python(pCode:String)
    extends ASTNode { self =>

    //TODO: This does not evaluat because parseExpressionMod expects a path -> wait for framework to offer according
    //      function
    lazy val pythonAST: ExpressionMod =
      com
      .github
      .python3parser
      .utilities
      .ASTParser
      .parseExpressionMod(pCode)

    override def toString: String = pCode

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)

    
  }

  object Python {
    /**
     * @return textual representation of python Code contained in `pCode` ,
     *         but wihtout leading and ending WhiteSpace characters.
     */
    def apply(pCode: String): Python = {
      val firstIndexWithoutWhiteSpaces =
        pCode
          .indexWhere(c => """[^\s]""".r.pattern.matcher(c.toString).matches())

      val lastIndexBeforeWhieSpacesBegin =
        pCode
          .lastIndexWhere(c => """[^\s]""".r.pattern.matcher(c.toString).matches()) + 1

      new Python(pCode.substring(
        if(firstIndexWithoutWhiteSpaces != -1) firstIndexWithoutWhiteSpaces else 0,
        if(lastIndexBeforeWhieSpacesBegin != -1) lastIndexBeforeWhieSpacesBegin else pCode.lastIndexOf(pCode.last + 1)
      ))
    }
  }
}
