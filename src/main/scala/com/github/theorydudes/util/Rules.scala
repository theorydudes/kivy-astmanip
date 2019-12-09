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

package com.github.theorydudes.util

import com.github.theorydudes.model.lines._
import com.github.theorydudes.model.ASTNode
import org.bitbucket.inkytonik.kiama.parsing.ListParsers
import org.bitbucket.inkytonik.kiama.util.Positions

class Rules(positions:Positions) extends ListParsers(positions) {
  /**
   * Match usual whitespace characters associated with `\s` token described at
   * [[https://docs.oracle.com/javase/7/docs/api/java/com.github.theorydudes.util/regex/Pattern.html]]
   * however tabs and newlines must not be consumed.
   *
   * @return Parser that matches whitespace characters except `\t` and `\n`
   */
  override def whitespace: Parser[Any] = regex("[\\s&&[^\\t\\n]]*".r)

  type IndentationParser[T] = Int => Parser[T]

  lazy val kv : Parser[TopLevel] = rep1(line) ^^ TopLevel

  lazy val line :Parser[RootNodeElement] = (
    directive
    | root
    | classP
    | comment
    | template
  )

  lazy val comment:Parser[Comment] = commenttext <~ "\n" ^^ Comment

  lazy val directive: Parser[Directive] = "#" ~> ":" ~>  """[^\n]+""".r <~ "\n" ^^ Directive

  lazy val root: Parser[Root] = widget(0) ^^ Root

  lazy val classP: Parser[ClassRule] =
    "<" ~> (classWidget <~ ">") ~ (":".? ~> classRuleTail) ^^ ClassRule

  lazy val template: Parser[Template] =
    "[" ~> classWidget ~ ("]" ~> ":".? ~> widgetBody(0)) ^^ Template

  lazy val classWidget : Parser[AutoClass] = widgetComp

  lazy val classRuleTail:Parser[List[ASTNode]] =
    widgetBody(0)

  lazy val widgetComp: Parser[AutoClass] =
    widgetList ~ ("@" ~> widgetBase).? ^^ AutoClass

  lazy val widgetBase : Parser[ClassListBase] =
    rep1sep(widgetName,"+") ^^ ClassListBase

  lazy val widgetList:Parser[ClassList] =
    rep1sep(widgetName,",") ^^ ClassList

  lazy val widgetName : Parser[KivyString] = (
    wname
    | "-" ~> wname ^^ (w => Reset(w.name))
  )

  lazy val widget: IndentationParser[Widget] = indentation =>
    wname ~ (":".? ~> widgetTail(indentation)) ^^ Widget

  lazy val widgetTail: IndentationParser[WidgetBody] = indentation => {
    widgetBody(indentation)
  }

  lazy val stmt: IndentationParser[WidgetBodyElement] = indentation => (
    widget(indentation)
    | canvas(indentation)
    | prop(indentation)
    | comment
    )

  lazy val widgetBody : IndentationParser[WidgetBody] = indentation => {
    rep1("\n") ~>  rep(indent(indentation+1) ~> stmt(indentation + 1))
  }


  lazy val canvas : IndentationParser[Canvas] = indentation => {
    canvasPre ~ (":" ~> canvasBody(indentation)) ^^ Canvas
  }

  lazy val canvasBody : IndentationParser[CanvasBody] = indentation => {
    rep1("\n") ~> rep1(indent(indentation+1) ~> canvasStmt(indentation+1))
  }

  lazy val canvasStmt:IndentationParser[CanvasBodyElement] = indentation => {
    instruction(indentation) | comment
  }

  lazy val instruction:IndentationParser[Instruction] = indentation => {
    wname ~ (":".? ~> instructionTail(indentation)) ^^ Instruction
  }

  lazy val instructionTail:IndentationParser[InstructionBody] = indentation => {
     instructionBody(indentation)
  }

  lazy val instructionBody:IndentationParser[InstructionBody] = indentation => {
    rep1("\n") ~> rep(indent(indentation+1) ~> instructionStmt(indentation+1))
  }

  lazy val instructionStmt:IndentationParser[InstructionBodyElement] = indentation => {
    prop(indentation) | comment
  }

  lazy val prop:IndentationParser[Property] = indentation => {
    name ~ (":" ~> propTail(indentation)) ^^ Property
  }

  lazy val propTail:IndentationParser[List[Python]] = indentation => {
    propBody(indentation) <~ "\n" | propValue <~ "\n" ^^ (p => List(p))
  }

  lazy val propBody:IndentationParser[List[Python]] = indentation => {
    rep1("\n") ~> rep1sep(indent(indentation+1) ~> propValue,"\n")
  }

  lazy val propValue:Parser[Python] = {
    """[^\n]+""".r ^^ (s => Python(s))
  }
  

  type Lexer = Parser[String]

  /**
   * @param indentation the amount of times `\t` is concatenated
   * @return a String containing the `indentation` amount of `\t` characters
   */
  private def ind (indentation:Int): String = "\t" * indentation

  /**
   * Indentation realized by constructing a parser that expects
   * the `indentation` amount of `\t` characters.
   */
  lazy val indent: Int =>  Lexer = indentation => ind(indentation)

  lazy val wname: Parser[WName] = """[A-Z][A-Za-z_0-9]*""".r ^^ WName

  lazy val name: Lexer = """[a-z_][A-Za-z_0-9]*""".r

  lazy val canvasPre : Parser[CanvasType.canvasType] =
    "canvas.after" ^^ (_ => CanvasType.After) |
    "canvas.before" ^^ (_ => CanvasType.Before) |
    "canvas" ^^ (_ => CanvasType.Regular)

  lazy val commenttext : Lexer = "#" ~> """[^\n]+""".r
}
