import com.github.theorydudes.model.lines
import com.github.theorydudes.model.lines.Python
import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import com.github.theorydudes.util.KivyParser.{File, Path}
import org.scalatest.FlatSpec
import com.github.theorydudes.util.KivyParser

class GuiSpec extends FlatSpec {
  "The AST of gui.kv" should " match the AST of the pretty-printed gui.kv" in {
    val ret = KivyParser(Path("src/test/scala/gui.kv")).topLevel
    assert(ret.isSuccess,"Initial parse of gui.kv was not successful.")
    val pretty = ret.pretty
    val ret2 = KivyParser(File(pretty)).topLevel
    assert(ret2.isSuccess, "Parse of pretty-printed AST failed.")
    assert(ret.get equals ret2.get)
  }

  "The AST of gui.kv" should " contain a Python-Code fragment with content ['Grey','000']" in {
    val ret = KivyParser(Path("src/test/scala/gui.kv")).topLevel
    val first = ret.get.collectFirst{
      case p@lines.Python(pCode) if pCode equals "['Grey','000']" => p
    }
    assert(first.isDefined)
    assert(first.get.pCode equals "['Grey','000']")
  }

  "The AST of gui.kv" should " replace node Python(['Grey','000']) by Python(['Red','000'])" in {
    val ret = KivyParser(Path("src/test/scala/gui.kv")).topLevel
    assert(!ret.get.exists(Python("['Red','000']")))
    val modified = ret.get.rewrite(
      {
        case Python("['Grey','000']") => Python("['Red','000']")
      }
    )
    assert(modified.exists(Python("['Red','000']")))
  }

  "get(indexOf(elem))" should " yield elem" in {
    val ret = KivyParser(Path("src/test/scala/gui.kv")).topLevel
    assert(ret.get.get(ret.get.indexOf(Python("['Grey','000']"))) equals Python("['Grey','000']"))
  }

  "Parsing" should "successfully terminate for all start points" in {
    val path : String => Path = s => Path(s"src/test/scala/$s.kv")
    lazy val retProp = KivyParser(path("prop"))
    lazy val retWidget = KivyParser(path("widget"))
    lazy val retTopLevel = KivyParser(path("gui"))
    lazy val retComment = KivyParser(path("comment"))
    lazy val retDirective = KivyParser(path("directive"))
    lazy val retRoot = KivyParser(path("root"))
    lazy val retClassRule = KivyParser(path("classRule"))
    lazy val retTemplate = KivyParser(path("template"))
    lazy val retCanvas = KivyParser(path("canvas"))
    lazy val retInstruction = KivyParser(path("instruction"))

    def failure[T] : ParseResult[T] => String = pr => s"Could not be parsed. Parsing result is: $pr"

    assert(retProp.prop.isSuccess,failure(retProp.prop.parseResult))
    assert(retWidget.widget.isSuccess,failure(retWidget.widget.parseResult))
    assert(retTopLevel.topLevel.isSuccess,failure(retTopLevel.topLevel.parseResult))
    assert(retComment.comment.isSuccess,failure(retComment.comment.parseResult))
    assert(retDirective.directive.isSuccess,failure(retDirective.directive.parseResult))
    assert(retRoot.root.isSuccess,failure(retRoot.root.parseResult))
    assert(retClassRule.classRule.isSuccess,failure(retClassRule.classRule.parseResult))
    assert(retTemplate.template.isSuccess,failure(retTemplate.template.parseResult))
    assert(retCanvas.canvas.isSuccess,failure(retCanvas.canvas.parseResult))
    assert(retInstruction.instruction.isSuccess,failure(retInstruction.instruction.parseResult))
  }
}
