package model

import model.lines.{ClassRuleNode, LineNode, WidgetNode}

case class KvModule(root:Option[WidgetNode] = None, classes:List[ClassRuleNode] = Nil)

object KvModule {
  def apply(lineNodes: List[LineNode]): KvModule = lineNodes.foldLeft(new KvModule()){
    case (kvModule,w:WidgetNode)=> kvModule.copy(root = Some(w))
    case (kvModule,c:ClassRuleNode) => kvModule.copy(classes = kvModule.classes ++ (c :: Nil))
    case (kvModule,_) => kvModule
  }
}
