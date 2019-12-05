package com.github.theorydudes.util

import com.github.theorydudes.model.lines.Widget
import com.github.theorydudes.model.ASTNode
import com.github.theorydudes.model.lines._
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

object KivyPrettyPrinter extends PrettyPrinter {

  def format[T<:ASTNode](t: T) : Document = {
    super.pretty(show(t))
  }

  def show (astNode:ASTNode) : Doc = astNode match {
    case TopLevel(rootLevelNodes) => vsep(rootLevelNodes.map(show))
    case Widget(name, widgetBody) => string(name.toString) <> char(':') <>
      nest(line <> vsep(widgetBody.map(show)))
    case s: KivyString => s.toString
    case ClassList(names) => ssep(names.map(n => string(n.toString)),',')
    case ClassListBase(names) => ssep(names.map(n => string(n.toString)),'+')
    case d:Directive => d.toString
    case AutoClass(widgetList, widgetBase) =>  show(widgetList) <> opt(widgetBase.map(n => '@' <> show(n)))
    case Template(classWidget, widgetBody) => '[' <> show(classWidget) <> ']' <> ':' <>
      nest(line <> vsep(widgetBody.map(show)))
    case ClassRule(classWidget, widgetBody) => '<' <> show(classWidget) <> '>' <> ':' <>
      nest(line <> vsep(widgetBody.map(show)))
    case Root(widget) => show(widget)
    case c:Comment => c.toString
    case Canvas(canvasType,canvasBody) => canvasType.toString <> ':' <>
      nest(line <> vsep(canvasBody.map(show)))
    case Instruction(name,instructionBody) => name.toString <> ':' <>
      nest(line <> vsep(instructionBody.map(show)))
    case Property(name,pythonBody) => name <> ':' <+>
      notNestedIfPossible(pythonBody.map(p => string(p.toString)))
  }

  def opt(optDoc:Option[Doc]):Doc = optDoc.getOrElse(emptyDoc)

  def notNestedIfPossible(docs:List[Doc]):Doc = docs.size match {
    case 1 => docs.head
    case _ => nest(line <> vsep(docs))
  }
}
