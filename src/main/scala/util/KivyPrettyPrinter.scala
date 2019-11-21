package util

import model.nodes.Widget
import model.{ASTNode, KvModule}
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter

object KivyPrettyPrinter extends PrettyPrinter {
  def pretty(kvModule: KvModule) : String = {
    throw new NotImplementedError()
  }

  def show (astNode:ASTNode) : Doc = astNode match {
    case Widget(name,widgetBody) =>throw new NotImplementedError()
  }
}
