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
}
