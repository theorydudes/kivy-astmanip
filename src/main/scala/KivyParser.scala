import model.KvModule
import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, Source, StringSource}
import util.PreProcessor

case class KivyParser(source:Source) {
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
      throw new IllegalArgumentException("There is either no file or path defined or there both are defined.")
    KivyParser(if(path != "")FileSource(path) else StringSource(file))
  }
}
