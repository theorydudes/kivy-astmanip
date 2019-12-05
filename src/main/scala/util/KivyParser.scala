package util

import model.ASTNode
import model.lines.{Canvas, ClassRule, Comment, Directive, Instruction, Property, Root, RootLevelNodes, Template, TopLevel, Widget}
import org.bitbucket.inkytonik.kiama.parsing
import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, ParseResult}
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, Source, StringSource}

case class KivyParser(source:Source) {

  case class KivyParserResult[T <: ASTNode ](parseResult: ParseResult[T]){
    def isSuccess: Boolean = parseResult match {
      case parsing.Success(_,_) => true
      case _ => false
    }

    def isFailure: Boolean = parseResult match {
      case parsing.Success(_,_) => false
      case _ => true
    }

    def get: T = parseResult match {
      case parsing.Success(result, next) => result
      case success: NoSuccess =>throw new IllegalStateException("No parsing result.")
    }

    def pretty:String = parseResult match {
      case parsing.Success(result, _) =>
        val doc = KivyPrettyPrinter.format(result)
        doc.layout
      case parsing.NoSuccess(_,_) =>
        throw new IllegalStateException("A non successful parsing result may not be pretty printed.")
    }
  }

  private lazy val parseInit = (new Rules(new Positions),PreProcessor(source.content))

  def topLevel : KivyParserResult[TopLevel] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.kv,parseInit._2.parserInput))

  def comment : KivyParserResult[Comment] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.comment,parseInit._2.parserInput))

  def directive : KivyParserResult[Directive] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.directive,parseInit._2.parserInput))

  def root : KivyParserResult[Root] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.root_rule,parseInit._2.parserInput))

  def classRule : KivyParserResult[ClassRule] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.class_rule,parseInit._2.parserInput))

  def template : KivyParserResult[Template] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.template_rule,parseInit._2.parserInput))

  def widget : KivyParserResult[Widget] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.widget(0),parseInit._2.parserInput))

  def canvas : KivyParserResult[Canvas] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.canvas(0),parseInit._2.parserInput))

  def prop : KivyParserResult[Property] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.prop(0),parseInit._2.parserInput))

  def instruction : KivyParserResult[Instruction] =
    KivyParserResult(parseInit._1.parseAll(parseInit._1.instruction(0),parseInit._2.parserInput))

}

object KivyParser {
  case class Path(p:String)
  case class File(f:String)
  def apply(path:Path):KivyParser = KivyParser(FileSource(path.p))
  def apply(file:File):KivyParser = KivyParser(StringSource(file.f))
}
