import model.KvModule
import org.bitbucket.inkytonik.kiama.parsing
import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, ParseResult}
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, Source, StringSource}
import util.PreProcessor

import scala.util.Success

case class KivyParser(source:Source) {

  case class KivyParserResult(parseResult: ParseResult[KvModule]){
    def isSuccess: Boolean = parseResult match {
      case parsing.Success(_,_) => true
      case _ => false
    }

    def isFailure: Boolean = parseResult match {
      case parsing.Success(_,_) => false
      case _ => true
    }



  }

  def parse : ParseResult[KvModule] = {
    val positions = new Positions
    val rules = new Rules(positions)
    val preProcessor = PreProcessor(source.content)
    rules.parseAll(rules.kv,preProcessor.parserInput)
  }
}

object KivyParser {
  def apply(path:String = "",file:String = ""):KivyParser = {
    if(file == "" && path == "" || file != "" && path != "")
      throw new IllegalArgumentException("There is either no file or path defined or both are defined.")
    KivyParser(if(path != "")FileSource(path) else StringSource(file))
  }
}
