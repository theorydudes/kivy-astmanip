import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}

object Main extends App {
  val positions = new Positions
  val rules = new Rules(positions)

  val input = StringSource("#:iuzawduzbshdbw asdw.asdwd")
  println(rules.parseAll(rules.directive,input))
}
