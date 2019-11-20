import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Parsers, Success}
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, StringSource}
import util.PreProcessor


object Main extends App {
  val positions = new Positions
  val rules = new Rules(positions)

  val regex = "[\\s&&[^\\t\\n]]*".r
  println(regex.pattern.matcher("  ").matches())
  val input = FileSource("src/main/scala/testWindows.kv")

  val preProcessor = PreProcessor(input.content)
  val result = rules.parseAll(rules.kv,preProcessor.parserInput)

  println()
  println()
  result match {
    case Success(result, next) => println(result)
    case success: NoSuccess =>
  }
}
