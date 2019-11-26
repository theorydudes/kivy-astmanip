import model.lines
import util.KivyParser.{File, Path}
import org.scalatest.FlatSpec
import util.KivyParser

class GuiSpec extends FlatSpec {
  "The AST of gui.kv" should " match the AST of the pretty-printed gui.kv" in {
    val ret = KivyParser(Path("src/test/scala/gui.kv")).parse
    assert(ret.isSuccess,"Initial parse of gui.kv was not successful.")
    val pretty = ret.pretty
    val ret2 = KivyParser(File(pretty)).parse
    assert(ret2.isSuccess, "Parse of pretty-printed AST failed.")
    assert(ret.get equals ret2.get)
  }

  "The AST of gui.kv" should " contain a Python-Code fragment with content ['Grey','000']" in {
    val ret = KivyParser(Path("src/test/scala/gui.kv")).parse
    val first = ret.get.collectFirst{
      case p@lines.Python(pCode) if pCode equals "['Grey','000']" => p
    }
    assert(first.isDefined)
    assert(first.get.pCode equals "['Grey','000']")
  }
}
