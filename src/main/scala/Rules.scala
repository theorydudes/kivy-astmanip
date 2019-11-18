import model.lines.{CanvasNode, CommentNode, DirectiveNode, InstructionNode, LineNode, PropertyNode, PythonNode, WidgetNode}
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

  /**
   * Match usual whitespace characters associated with `\s` token described at
   * [[https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html]]
   * however tabs and newlines must not be consumed.
   *
   * @return Parser that matches regular expression `( \x0B\f\r)*`
   */
  override def whitespace: Parser[Any] = """( \x0B\f\r)*""".r

  type IndentationParser[T] = Int => Parser[T]

  lazy val line :Parser[LineNode] = (
    directive
    | root_rule
  )

  lazy val comment:Parser[CommentNode] = lexer.commenttext ^^ CommentNode

  lazy val directive: Parser[DirectiveNode] = "#" ~> ":" ~>  """[^\n]+""".r ^^ DirectiveNode

  lazy val root_rule: Parser[WidgetNode] = widget(0)

  lazy val widget: IndentationParser[WidgetNode] = indentation => (
      lexer.wname ~ (":".? ~> widget_tail(indentation)) ^^ {
       case n ~ b => WidgetNode(n,b)
      }
    )

  lazy val widget_tail: IndentationParser[List[LineNode]] = indentation => {
    widget_body(indentation) | "\n" ^^ (_ => Nil)
  }

  lazy val stmt: IndentationParser[LineNode] = indentation => (
    widget(indentation)
    )

  lazy val widget_body : IndentationParser[List[LineNode]] = indentation => {
    rep1("\n") ~>  rep1(lexer.indent(indentation+1) ~> stmt(indentation + 1))
  }


  lazy val canvas : IndentationParser[CanvasNode] = indentation => {
    lexer.canvas ~> ":" ~> canvas_body(indentation) ^^ CanvasNode
  }
  
  lazy val canvas_body : IndentationParser[List[LineNode]] = indentation => {
    rep1("\n") ~> rep1(lexer.indent(indentation+1) ~> canvas_stmt(indentation+1))
  }

  lazy val canvas_stmt:IndentationParser[LineNode] = indentation => {
    instruction(indentation)
  }

  lazy val instruction:IndentationParser[LineNode] = indentation => {
    lexer.wname ~ (":".? ~> instruction_tail(indentation)) ^^ {case n ~ b => InstructionNode(n,b)}
  }

  lazy val instruction_tail:IndentationParser[List[LineNode]] = indentation => {
    "\n" ^^ (_ => Nil) | instruction_body(indentation)
  }

  lazy val instruction_body:IndentationParser[List[LineNode]] = indentation => {
    rep1("\n") ~> rep1(lexer.indent(indentation+1) ~> instruction_stmt(indentation+1))
  }

  //TODO: option blank was ignored here, add if needed
  lazy val instruction_stmt:IndentationParser[LineNode] = indentation => {
    prop(indentation) | comment
  }

  lazy val prop:IndentationParser[PropertyNode] = indentation => {
    lexer.name ~ (":" ~> prop_tail(indentation)) ^^ PropertyNode
  }

  lazy val prop_tail:IndentationParser[List[PythonNode]] = indentation => {
    prop_value <~ "\n" ^^ (p => List(p)) | prop_body(indentation)
  }

  lazy val prop_body:IndentationParser[List[PythonNode]] = indentation => {
    rep1("\n") ~> rep1sep(lexer.indent(indentation+1) ~> prop_value,"\n")
  }

  lazy val prop_value:Parser[PythonNode] = {
    """[^\n]+""".r ^^ PythonNode
  }

  object lexer {

    type Lexer = Parser[String]

    /**
     * @param indentation the amount of times `\t` is concatenated
     * @return a String containing the `indentation` amount of `\t` characters
     */
    private def ind (indentation:Int): String = "\t" * indentation

    /**
     * Indentation realized by constructing a parser that expects
     * the `indentation` amount of `\t` characters.
     */
    lazy val indent: Int =>  Lexer = indentation => ind(indentation)

    lazy val wname: Lexer = """[A-Z][A-Za-z_0-9]*""".r

    lazy val name: Lexer = """[a-z_][A-Za-z_0-9]*""".r

    lazy val canvas : Lexer =  "canvas" | "canvas.before" | "canvas.after"

    lazy val commenttext : Lexer = "#" ~> """[^\n]+""".r
  }
}
