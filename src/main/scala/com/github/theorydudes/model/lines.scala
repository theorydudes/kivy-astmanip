/*
 * Copyright 2019 com.github.theorydudes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.theorydudes.model

import com.github.python3parser.model.mods.ExpressionMod
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.{kiama => Kiama}

object lines {
  trait WidgetBodyElement extends ASTNode {
    override def traverseAndApply(s: Strategy): WidgetBodyElement
  }
  trait InstructionBodyElement extends ASTNode {
    override def traverseAndApply(s: Strategy): InstructionBodyElement
  }
  trait CanvasBodyElement extends ASTNode {
    override def traverseAndApply(s: Strategy): CanvasBodyElement
  }
  trait RootNodeElement extends ASTNode {
    override def traverseAndApply(s: Strategy): RootNodeElement
  }

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

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(rootLevelNodes)(z)(op),self)

    override def traverseAndApply(s: Strategy): TopLevel =
      Kiama
        .rewriting
        .Rewriter
        .rewrite(s)(TopLevel(rootLevelNodes.map(_.traverseAndApply(s))))
  }

  case class Directive(directive:String)
    extends RootNodeElement {self =>

    override def toString: String = s"#:$directive"

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = op(z,self)

    override def traverseAndApply(s: Strategy): RootNodeElement =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(self)
  }

  case class Root(widget:Widget)
    extends RootNodeElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = {
      op(widget.foldLeft(z)(op),self)
    }

    override def traverseAndApply(s: Strategy): RootNodeElement =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(Root(widget.traverseAndApply(s).asInstanceOf[Widget]))
  }
  case class Widget(name:WName, widgetBody:WidgetBody)
    extends WidgetBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = {
      val foldedName = name.foldLeft(z)(op)
      val foldedBody =  foldList(widgetBody)(foldedName)(op)
      op(foldedBody,self)
    }

    override def traverseAndApply(s: Strategy): WidgetBodyElement =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(Widget(name.traverseAndApply(s).asInstanceOf[WName],widgetBody.map(_.traverseAndApply(s))))
  }
  case class Canvas(canvasType: CanvasType.canvasType,canvasBody:CanvasBody)
    extends WidgetBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B = {
      val foldedBody = foldList(canvasBody)(z)(op)
      op(foldedBody,self)
    }

    override def traverseAndApply(s: Strategy): WidgetBodyElement =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(Canvas(canvasType,canvasBody.map(_.traverseAndApply(s))))
  }

  case class Instruction(name:WName, instructionBody:InstructionBody)
    extends CanvasBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(instructionBody)(name.foldLeft(z)(op))(op),self)

    override def traverseAndApply(s: Strategy): CanvasBodyElement =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(Instruction(name.traverseAndApply(s).asInstanceOf[WName],instructionBody.map(_.traverseAndApply(s))))
  }

  case class Property(name:String, propertyBody:List[Python])
    extends WidgetBodyElement with InstructionBodyElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(propertyBody)(z)(op),self)

    override def traverseAndApply(s: Strategy): Property =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(Property(name,propertyBody.map(_.traverseAndApply(s))))
  }

  case class Comment(comment:String)
    extends WidgetBodyElement with InstructionBodyElement with CanvasBodyElement with RootNodeElement { self =>

    override def toString: String = s"#$comment"

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)

    override def traverseAndApply(s: Strategy): Comment =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(self)
  }

  case class ClassRule(classWidget:AutoClass, widgetBody:List[ASTNode])
    extends RootNodeElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(widgetBody)(classWidget.foldLeft(z)(op))(op),self)

    override def traverseAndApply(s: Strategy): RootNodeElement =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(ClassRule(classWidget.traverseAndApply(s),widgetBody.map(_.traverseAndApply(s))))
  }

  case class ClassList(names:List[KivyString])
    extends ASTNode { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(names)(z)(op),self)

    override def traverseAndApply(s: Strategy): ClassList =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(ClassList(names.map(_.traverseAndApply(s))))
  }

  case class ClassListBase(names:List[KivyString])
    extends ASTNode { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(names)(z)(op),self)

    override def traverseAndApply(s: Strategy): ClassListBase =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(ClassListBase(names.map(_.traverseAndApply(s))))
  }

  case class AutoClass(widgetList:ClassList, widgetBase:Option[ClassListBase])
    extends ASTNode { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldOption(widgetBase)(widgetList.foldLeft(z)(op))(op),self)

    override def traverseAndApply(s: Strategy): AutoClass =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(AutoClass(widgetList.traverseAndApply(s),widgetBase.map(_.traverseAndApply(s))))

  }

  case class Template(classWidget:AutoClass, widgetBody:List[ASTNode])
    extends RootNodeElement { self =>

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(foldList(widgetBody)(classWidget.foldLeft(z)(op))(op),self)

    override def traverseAndApply(s: Strategy): RootNodeElement =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(Template(classWidget.traverseAndApply(s),widgetBody.map(_.traverseAndApply(s))))
  }

  sealed trait KivyString extends ASTNode {
    override def traverseAndApply(s: Strategy): KivyString
  }

  case class WName(name:String)
    extends KivyString { self =>
    override def toString: String = name

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)

    override def traverseAndApply(s: Strategy): KivyString =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(self)
  }

  case class Reset(name:String)
    extends KivyString { self =>

    override def toString: String = s"-$name"

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)

    override def traverseAndApply(s: Strategy): KivyString =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(self)
  }

  case class Python(pCode:String)
    extends ASTNode { self =>

    lazy val pythonAST: ExpressionMod =
      com
      .github
      .python3parser
      .utilities
      .ASTParser
      .parseExpressionModWithCode(pCode)

    override def toString: String = pCode

    override def foldLeft[B](z: B)(op: (B, ASTNode) => B): B =
      op(z,self)

    override def traverseAndApply(s: Strategy): Python =
      Kiama
      .rewriting
      .Rewriter
      .rewrite(s)(self)
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
