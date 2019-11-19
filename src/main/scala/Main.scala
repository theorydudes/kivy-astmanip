import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, StringSource}
import util.PreProcessor


object Main extends App {
  val positions = new Positions
  val rules = new Rules(positions)

  val s = "GuiLayout:\n\tSomeWidget:\n\t\tcanvas:\n\t\t\tInstruction:\n\t\t\t\tprop1:iauwbdhsbd\n"

  val fail = "Failure(string matching regex '[A-Z][A-Za-z_0-9]*' expected but '\t' found,Input(StringSource(GuiLayout:\n\tSomeWidget:\n\t\tcanvas:\n\t\t\tInstruction\n\tSomeWidget2:\n\t\tcanvas:\n\t\t\tInstruction2:\n\t\t\t\tprop1:somepythoncode=somepythoncode),93))"

  val input = FileSource("src/main/scala/test.kv")

  val preProcessor = PreProcessor(input.content)
  println(preProcessor.post)
  val input2 = StringSource(s)
  println(rules.parseAll(rules.kv,preProcessor.parserInput))
}
