import model.lines.{Directive, LineNode}
import org.bitbucket.inkytonik.kiama.parsing.{Failure, Input, ListParsers}
import org.bitbucket.inkytonik.kiama.util.Positions

import scala.util.matching.Regex

//TODO: Constraint: Nur spaces außerhalb von Indentation. Eventuell manuell vorher ersetzen
/*TODO: Indentation ist doch wichtig, da man sonst folgendes zum Beispiel nicht unterscheiden kann:
        CanvasInstruction1:
            canvasprop1: value1
        CanvasInstruction2:
            canvasprop2: value2
 */
//TODO: Idee: Jeder Parser bekommt zusätzlich zu der parser rule die aktuell erwartete Indentation
class Rules(positions:Positions) extends ListParsers(positions) {

  lazy val line :Parser[LineNode] = (
    directive
    | root_rule
  )

  lazy val directive: Parser[Directive] = "#" ~> ":" ~>  """[^\n].+""".r ^^ Directive

  lazy val root_rule = widget(0)

  lazy val widget: Int => Parser[] = expectedIndentation => (
    lexer.wname <~ ":".?
      | lexer.wname ~ (":".? ~> widget_body(expectedIndentation + 1))
    )

  lazy val widget_body : Int => Parser[] = expectedIndentation => {
    rep1("\n") ~> lexer.indent(expectedIndentation) ~> rep1(stmt)
  }

  lazy val stmt = widget

  object lexer {

    /**
     * @param expectedIndentation the amount of times `\t` is concatenated
     * @return a String containing the `expectedIndentation` amount of `\t` characters
     */
    private def ind (expectedIndentation:Int): String = "\t" * expectedIndentation

    /**
     * Indentation realized by constructing a parser that expects
     * the `expectedIndentation` amount of `\t` characters.
     */
    lazy val indent: Int =>  Parser[String] = expectedIndentation => ind(expectedIndentation)

    lazy val wname: Parser[String] = """[A-Z][A-Za-z_0-9]""".r
  }
}
