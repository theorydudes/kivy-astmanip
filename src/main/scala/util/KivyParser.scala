package util

import model.lines.{RootLevelNodes, TopLevel}
import org.bitbucket.inkytonik.kiama.parsing
import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, ParseResult}
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, Source, StringSource}

case class KivyParser(source:Source) {

  case class KivyParserResult(parseResult: ParseResult[TopLevel]){
    def isSuccess: Boolean = parseResult match {
      case parsing.Success(_,_) => true
      case _ => false
    }

    def isFailure: Boolean = parseResult match {
      case parsing.Success(_,_) => false
      case _ => true
    }

    def get: TopLevel = parseResult match {
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

  def parse : KivyParserResult = {
    val positions = new Positions
    val rules = new Rules(positions)
    val preProcessor = PreProcessor(source.content)
    KivyParserResult(rules.parseAll(rules.kv,preProcessor.parserInput))
  }
}

object KivyParser {
  case class Path(p:String)
  case class File(f:String)
  def apply(path:Path):KivyParser = KivyParser(FileSource(path.p))
  def apply(file:File):KivyParser = KivyParser(StringSource(file.f))
}
