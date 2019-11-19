import model.lines.{AutoClassNode, BlankNode, CanvasNode, ClassListNode, ClassRuleNode, CommentNode, DirectiveNode, InstructionNode, LineNode, PropertyNode, PythonNode, ResetRuleNode, StringRuleNode, WNameRuleNode, WidgetNode}
import org.bitbucket.inkytonik.kiama.parsing.{Failure, Input, ListParsers, Parsers}
import org.bitbucket.inkytonik.kiama.util.Positions
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.util.matching.Regex

//TODO: Constraint: Nur spaces außerhalb von Indentation. Eventuell manuell vorher ersetzen
/*TODO: Indentation ist doch wichtig, da man sonst folgendes zum Beispiel nicht unterscheiden kann:
        CanvasInstruction1:
            canvasprop1: value1
        CanvasInstruction2:
            canvasprop2: value2
 */
//TODO: Auf linksfaktorisierung prüfen
//TODO: Blanks hinzufügen, macht allerdings linksfaktorisierung kaputt, aufpassen!
class Rules(positions:Positions) extends ListParsers(positions) {

  /**
   * Match usual whitespace characters associated with `\s` token described at
   * [[https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html]]
   * however tabs and newlines must not be consumed.
   *
   * @return Parser that matches whitespace characters except `\t` and `\n`
   */
  override def whitespace: Parser[Any] = regex("[\\s&&[^\\t\\n]]*".r)

  type IndentationParser[T] = Int => Parser[T]

  lazy val kv : Parser[List[LineNode]] = rep1(line | blank ^^ (_ => BlankNode))

  lazy val line :Parser[LineNode] = (
    directive
    | root_rule
    | class_rule
    | comment
  )

  lazy val blank:Parser[LineNode] = "\n" ^^ (_ => BlankNode)

  lazy val comment:Parser[CommentNode] = commenttext <~ "\n" ^^ CommentNode

  lazy val directive: Parser[DirectiveNode] = "#" ~> ":" ~>  """[^\n]+""".r ^^ DirectiveNode

  lazy val root_rule: Parser[WidgetNode] = widget(0)

  lazy val class_rule: Parser[ClassRuleNode] = "<" ~> (widget_comp <~ ">") ~ (":".? ~> class_rule_tail) ^^ ClassRuleNode

  lazy val class_rule_tail:Parser[List[LineNode]] =
    widget_body(0)

  lazy val widget_comp: Parser[AutoClassNode] =
    widget_list ~ ("@" ~> widget_base).? ^^ AutoClassNode

  lazy val widget_base : Parser[ClassListNode] =
    rep1sep(widget_name,"+") ^^ ClassListNode

  lazy val widget_list:Parser[ClassListNode] =
    rep1sep(widget_name,",") ^^ ClassListNode

  lazy val widget_name : Parser[StringRuleNode] = (
    wname
    | "-" ~> wname ^^ (w => ResetRuleNode(w.name))
  )

  lazy val widget: IndentationParser[WidgetNode] = indentation => wname ~ (":".? ~> widget_tail(indentation)) ^^ {
   case n ~ b => WidgetNode(n,b)
  }

  lazy val widget_tail: IndentationParser[List[LineNode]] = indentation => {
    widget_body(indentation)
  }

  lazy val stmt: IndentationParser[LineNode] = indentation => (
    widget(indentation)
    | canvas(indentation)
    | prop(indentation)
    | comment
    )

  lazy val widget_body : IndentationParser[List[LineNode]] = indentation => {
    rep1("\n") ~>  rep(indent(indentation+1) ~> stmt(indentation + 1))
  }


  lazy val canvas : IndentationParser[CanvasNode] = indentation => {
    canvasPre ~> ":" ~> canvas_body(indentation) ^^ CanvasNode
  }
  
  lazy val canvas_body : IndentationParser[List[LineNode]] = indentation => {
    rep1("\n") ~> rep1(indent(indentation+1) ~> canvas_stmt(indentation+1))
  }

  lazy val canvas_stmt:IndentationParser[LineNode] = indentation => {
    instruction(indentation) | comment
  }

  lazy val instruction:IndentationParser[LineNode] = indentation => {
    wname ~ (":".? ~> instruction_tail(indentation)) ^^ InstructionNode
  }

  lazy val instruction_tail:IndentationParser[List[LineNode]] = indentation => {
     instruction_body(indentation)
  }

  lazy val instruction_body:IndentationParser[List[LineNode]] = indentation => {
    rep1("\n") ~> rep(indent(indentation+1) ~> instruction_stmt(indentation+1))
  }

  lazy val instruction_stmt:IndentationParser[LineNode] = indentation => {
    prop(indentation) | comment
  }

  lazy val prop:IndentationParser[PropertyNode] = indentation => {
    name ~ (":" ~> prop_tail(indentation)) ^^ PropertyNode
  }

  lazy val prop_tail:IndentationParser[List[PythonNode]] = indentation => {
    prop_body(indentation) <~ "\n" | prop_value <~ "\n" ^^ (p => List(p))
  }

  lazy val prop_body:IndentationParser[List[PythonNode]] = indentation => {
    rep1("\n") ~> rep1sep(indent(indentation+1) ~> prop_value,"\n")
  }

  lazy val prop_value:Parser[PythonNode] = {
    """[^\n]+""".r ^^ PythonNode
  }
  

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

  lazy val wname: Parser[WNameRuleNode] = """[A-Z][A-Za-z_0-9]*""".r ^^ WNameRuleNode

  lazy val name: Lexer = """[a-z_][A-Za-z_0-9]*""".r

  lazy val canvasPre : Lexer =  "canvas" | "canvas.before" | "canvas.after"

  lazy val commenttext : Lexer = "#" ~> """[^\n]+""".r
}
