import java.util

import com.github.python3parser.model.Identifier
import com.github.python3parser.model.expr.Expression
import com.github.python3parser.model.expr.atoms.{Name, Num, Str}
import com.github.python3parser.model.expr.datastructures.ListExpr
import com.github.python3parser.model.mods.ExpressionMod
import com.github.theorydudes.model.ASTNode
import com.github.theorydudes.model.model.{Property, Python, Root, TopLevel, WName, Widget}
import com.github.theorydudes.util.KivyParser
import com.github.theorydudes.util.KivyParser.{File, Path}
import org.bitbucket.inkytonik.kiama.==>
import org.scalatest.FlatSpec
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._

/**
 * Test python nodes that are directly parsed and python nodes that are instantiated
 * using the `fromPythonAST`-method in [[com.github.theorydudes.model.model.Python]]
 * on equality.
 *
 * However since strings are pretty printed by the python3parser framework, spacing is out of control.
 * Therefore to compare strings, all whitespace characters are removed and the sequence of symbols are compared.
 * For example a python-list [1,2,3] is printed [1, 2, 3] but is obviously the same list.
 *
 */
class PythonNodeSpec extends FlatSpec {

  val elts = new util.ArrayList[Expression]
  elts.add(new Num("1"))
  elts.add(new Num("2"))
  elts.add(new Num("3"))
  elts.add(new Num("4"))
  elts.add(new Num("5"))
  elts.add(new Num("6"))
  elts.add(new Num("7"))

  lazy val pythonTestAST = TopLevel(
    List(Root(
      Widget(
        WName("TestPython"),
        List(
          Property("id",List(Python.fromPythonAST(new ExpressionMod(new Name(new Identifier("my_personal_id")))))),
          Property("scale",List(Python.fromPythonAST(new ExpressionMod(new ListExpr(elts))))),
          Property("someString",List(Python.fromPythonAST(new ExpressionMod(new Str("\"foo\""))))),
          Property("multiline",List(
            Python.fromPythonAST(new ExpressionMod(new Str("\"line1\""))),
            Python.fromPythonAST(new ExpressionMod(new Str("\"line2\""))))
          )
        )
      )
    ))
  )

  val parsed = KivyParser(Path("src/test/scala/python.kv")).topLevel
  assert(parsed.isSuccess,s"parsing of python.kv failed with message: ${parsed.parseResult.toMessage}")
  val pythonTestToCompare = pythonTestAST.traverseAndApply(rule[ASTNode]({
    case Python(s) => Python(s.replaceAll("\\s",""))
  }))
  val parsedToCompare = parsed.get.traverseAndApply(rule[ASTNode]({
    case Python(s) => Python(s.replaceAll("\\s",""))
  }))
  assert(parsedToCompare equals pythonTestToCompare, s"manually created parse tree and ast of" +
    s" python.kv are not equal with message: ${parsed.get.pretty} does not equals ${pythonTestAST.pretty}")
}
