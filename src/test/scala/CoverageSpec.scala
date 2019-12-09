import com.github.theorydudes.util.KivyParser
import com.github.theorydudes.util.KivyParser.{File, Path}
import org.scalatest.FlatSpec

class CoverageSpec extends FlatSpec {
  "The AST of coverage.kv" should "be identical to the AST of the prettyPrinted coverage.kv file" in {
    val ret1 = KivyParser(Path("src/test/scala/coverage.kv")).topLevel
    assert(ret1.isSuccess,s"Coverage.kv could ne be parsed, with error: " + ret1.parseResult)
    val ret1Pretty = ret1.pretty
    val ret2 = KivyParser(File(ret1Pretty)).topLevel
    assert(ret2.isSuccess)
    assert(ret1.get equals ret2.get)
  }
}
