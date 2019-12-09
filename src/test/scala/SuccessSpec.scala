import com.github.theorydudes.util.KivyParser
import com.github.theorydudes.util.KivyParser.Path
import org.bitbucket.inkytonik.kiama.parsing.ParseResult
import org.scalatest.FlatSpec

class SuccessSpec extends FlatSpec{
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
