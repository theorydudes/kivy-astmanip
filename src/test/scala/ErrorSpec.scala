import com.github.theorydudes.util.KivyParser
import com.github.theorydudes.util.KivyParser.{File, Path}
import org.scalatest.FlatSpec

class ErrorSpec extends FlatSpec {
  lazy val notKivy = "not a kivy file"
  lazy val emptyKivy = ""

  "parsing a file that does not match the kivy language" should "lead to a parser failure" in {
    val parseResult = KivyParser(File(notKivy)).topLevel
    assert(parseResult.isFailure)
    assert(!parseResult.isSuccess)
    assertThrows[IllegalStateException](parseResult.get)
  }

  "parsing an empty file" should "not pass the preprocessor" in {
    assertThrows[IllegalArgumentException](KivyParser(File(emptyKivy)).topLevel)
  }

  "parsing a file that doe match the kivy language" should "not lead to a parser failure" in {
    val parseResult = KivyParser(Path("src/test/scala/gui.kv")).topLevel
    assert(!parseResult.isFailure)
  }
}
