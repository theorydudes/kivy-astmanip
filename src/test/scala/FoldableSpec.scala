import com.github.theorydudes.model.{ASTNode, lines}
import com.github.theorydudes.model.lines.{Instruction, Property, Python, WName, Widget}
import com.github.theorydudes.util.KivyParser
import com.github.theorydudes.util.KivyParser.Path
import org.scalatest.FlatSpec

class FoldableSpec extends FlatSpec {

  val pathToGui: Path = Path("src/test/scala/gui.kv")
  val pathToCov: Path = Path("src/test/scala/coverage.kv")

  "The AST of gui.kv" should " contain a Python-Code fragment with content ['Grey','000']" in {
    val ret = KivyParser(pathToGui).topLevel
    val first = ret.get.collectFirst{
      case p@lines.Python(pCode) if pCode equals "['Grey','000']" => p
    }
    assert(first.isDefined)
    assert(first.get.pCode equals "['Grey','000']")
  }

  "The AST of gui.kv" should " replace node Python(['Grey','000']) by Python(['Red','000'])" in {
    val ret = KivyParser(pathToGui).topLevel
    assert(!ret.get.exists(Python("['Red','000']")))
    val modified = ret.get.rewrite(
      {
        case Python("['Grey','000']") => Python("['Red','000']")
      }
    )
    assert(modified.exists(Python("['Red','000']")))
  }

  "get(indexOf(elem))" should " yield elem" in {
    val ret = KivyParser(pathToGui).topLevel
    assert(ret.get.get(ret.get.indexOf(Python("['Grey','000']"))) equals Python("['Grey','000']"))
  }

  "forall on gui.kv" should "validate that there are no instances of Instruction" in {
    val ret = KivyParser(pathToGui).topLevel
    assert(ret.get.forall{case _:Instruction => false case _ => true})
  }

  "forall on coverage.kv" should "validate that if there is a property it must not be named stupidProp" in {
    val ret = KivyParser(pathToCov).topLevel
    assert(ret.get.forall{case Property("stupidProp",_) => false case _ => true})
  }

  "reduceLeftOption on coverage.kv" should "validate that there is no Instruction with name Foo" in {
    val ret = KivyParser(pathToCov).topLevel
    assert(ret.get.reduceLeftToOption({case Instruction(WName("Foo"),_) => true case _ => false})({
      case (_,Instruction(WName("Foo"),_)) => false
      case _ => true
    }).get)
  }

  "find on coverage.kv" should "validate that there is an Instruction with name Color" in {
    val ret = KivyParser(pathToCov).topLevel
    assert(ret.get.find{
      case Instruction(WName("Color"),_) => true
      case _ => false
    }.isDefined)
  }

  "filter on coverage.kv" should "provide a list of astNodes that are Instructions" in {
    val ret = KivyParser(pathToCov).topLevel
    val filtered = ret.get.filter{
      case _:Instruction => true
      case _ => false
    }
    assert(filtered.size equals filtered.collect{case i:Instruction => i}.size)
  }
}
