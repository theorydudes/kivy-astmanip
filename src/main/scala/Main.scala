import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Parsers, Success}
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, StringSource}
import util.PreProcessor


object Main extends App {
  println(KivyParser("src/main/scala/testWindows.kv").parse)
}
