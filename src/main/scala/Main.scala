import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, StringSource}

object Main extends App {
  val positions = new Positions
  val rules = new Rules(positions)

  val s = "GuiLayout:\n\tCanvas:\n\t\tCanvas:\n"

  val fail = "Failure(string matching regex '[A-Z][A-Za-z_0-9].*' expected but '\t' found,Input(StringSource(GuiLayout:\n\tCanvas:\n\tCanvas:),20))"

  val input = FileSource("src/main/scala/test.kv")
  val input2 = StringSource(s)
  println(rules.parseAll(rules.widget(0),input))
}
