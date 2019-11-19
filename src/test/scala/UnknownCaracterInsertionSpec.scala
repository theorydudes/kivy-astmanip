import org.bitbucket.inkytonik.kiama.parsing.Success
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions}
import org.scalatest.FlatSpec
import util.PreProcessor

/**
 * When using a file created on Mac parsing fails on Windows
 */

class UnknownCaracterInsertionSpec extends FlatSpec {
  val positions = new Positions
  val rules = new Rules(positions)

  val input = FileSource("src/test/scala/testMac.kv")
  val input2 = FileSource("src/test/scala/testWindows.kv")
  val preProcessor = PreProcessor(input.content)

  "testMac.kv" should "parse and be identical to testWindows.kv" in {
    assert(rules.parseAll(rules.kv,input).asInstanceOf[Success[_]] equals rules.parseAll(rules.kv,input2).asInstanceOf[Success[_]])
  }
}
