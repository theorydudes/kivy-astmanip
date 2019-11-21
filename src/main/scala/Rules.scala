import model.{ASTNode, KvModule}
import model.lines.{AutoClass, Canvas, CanvasBody, CanvasBodyElement, ClassList, ClassRule, Comment, Directive, Instruction, InstructionBody, InstructionBodyElement, KivyString, Property, Python, Reset, Template, WName, Widget, WidgetBody, WidgetBodyElement}
import org.bitbucket.inkytonik.kiama.parsing.ListParsers
import org.bitbucket.inkytonik.kiama.util.Positions

//TODO: Auf linksfaktorisierung prüfen
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

  lazy val kv : Parser[KvModule] = rep1(line ) ^^ {
    l => KvModule(l)
  }

  lazy val line :Parser[ASTNode] = (
    directive
    | root_rule
    | class_rule
    | comment
    | template_rule
  )

  lazy val comment:Parser[Comment] = commenttext <~ "\n" ^^ Comment

  lazy val directive: Parser[Directive] = "#" ~> ":" ~>  """[^\n]+""".r ^^ Directive

  lazy val root_rule: Parser[Widget] = widget(0)

  lazy val class_rule: Parser[ClassRule] =
    "<" ~> (class_widget <~ ">") ~ (":".? ~> class_rule_tail) ^^ ClassRule

  lazy val template_rule: Parser[Template] =
    "[" ~> class_widget ~ ("]" ~> ":".? ~> widget_body(0)) ^^ Template

  lazy val class_widget : Parser[AutoClass] = widget_comp

  lazy val class_rule_tail:Parser[List[ASTNode]] =
    widget_body(0)

  lazy val widget_comp: Parser[AutoClass] =
    widget_list ~ ("@" ~> widget_base).? ^^ AutoClass

  lazy val widget_base : Parser[ClassList] =
    rep1sep(widget_name,"+") ^^ ClassList

  lazy val widget_list:Parser[ClassList] =
    rep1sep(widget_name,",") ^^ ClassList

  lazy val widget_name : Parser[KivyString] = (
    wname
    | "-" ~> wname ^^ (w => Reset(w.name))
  )

  lazy val widget: IndentationParser[Widget] = indentation =>
    wname ~ (":".? ~> widget_tail(indentation)) ^^ {
   case n ~ b => Widget(n,b)
  }

  lazy val widget_tail: IndentationParser[WidgetBody] = indentation => {
    widget_body(indentation)
  }

  lazy val stmt: IndentationParser[WidgetBodyElement] = indentation => (
    widget(indentation)
    | canvas(indentation)
    | prop(indentation)
    | comment
    )

  lazy val widget_body : IndentationParser[WidgetBody] = indentation => {
    rep1("\n") ~>  rep(indent(indentation+1) ~> stmt(indentation + 1))
  }


  lazy val canvas : IndentationParser[Canvas] = indentation => {
    canvasPre ~> ":" ~> canvas_body(indentation) ^^ Canvas
  }
  
  lazy val canvas_body : IndentationParser[CanvasBody] = indentation => {
    rep1("\n") ~> rep1(indent(indentation+1) ~> canvas_stmt(indentation+1))
  }

  lazy val canvas_stmt:IndentationParser[CanvasBodyElement] = indentation => {
    instruction(indentation) | comment
  }

  lazy val instruction:IndentationParser[Instruction] = indentation => {
    wname ~ (":".? ~> instruction_tail(indentation)) ^^ Instruction
  }

  lazy val instruction_tail:IndentationParser[InstructionBody] = indentation => {
     instruction_body(indentation)
  }

  lazy val instruction_body:IndentationParser[InstructionBody] = indentation => {
    rep1("\n") ~> rep(indent(indentation+1) ~> instruction_stmt(indentation+1))
  }

  lazy val instruction_stmt:IndentationParser[InstructionBodyElement] = indentation => {
    prop(indentation) | comment
  }

  lazy val prop:IndentationParser[Property] = indentation => {
    name ~ (":" ~> prop_tail(indentation)) ^^ Property
  }

  lazy val prop_tail:IndentationParser[List[Python]] = indentation => {
    prop_body(indentation) <~ "\n" | prop_value <~ "\n" ^^ (p => List(p))
  }

  lazy val prop_body:IndentationParser[List[Python]] = indentation => {
    rep1("\n") ~> rep1sep(indent(indentation+1) ~> prop_value,"\n")
  }

  lazy val prop_value:Parser[Python] = {
    """[^\n]+""".r ^^ Python
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

  lazy val wname: Parser[WName] = """[A-Z][A-Za-z_0-9]*""".r ^^ WName

  lazy val name: Lexer = """[a-z_][A-Za-z_0-9]*""".r

  lazy val canvasPre : Lexer =  "canvas" | "canvas.before" | "canvas.after"

  lazy val commenttext : Lexer = "#" ~> """[^\n]+""".r
}
