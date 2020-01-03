import com.github.theorydudes.model.model
import com.github.theorydudes.model.model.Python
import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import com.github.theorydudes.util.KivyParser.{File, Path}
import org.scalatest.FlatSpec
import com.github.theorydudes.util.KivyParser

class GuiSpec extends FlatSpec {
  "The AST of gui.kv" should " be equal to the AST of the pretty-printed gui.kv" in {
    val ret = KivyParser(Path("src/test/scala/gui.kv")).topLevel
    assert(ret.isSuccess,"Initial parse of gui.kv was not successful.")
    val pretty = ret.get.pretty
    val ret2 = KivyParser(File(pretty)).topLevel
    assert(ret2.isSuccess, "Parse of pretty-printed AST failed.")
    assert(ret.get equals ret2.get)
  }
}
