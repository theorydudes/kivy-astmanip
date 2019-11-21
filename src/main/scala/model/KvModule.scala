package model

import model.nodes.{ClassRule, Widget}

case class KvModule(root:Option[Widget] = None, classes:List[ClassRule] = Nil,rawNodes:List[ASTNode] = Nil)

object KvModule {
  def apply(lineNodes: List[ASTNode]): KvModule = lineNodes.foldLeft(new KvModule()){
    case (kvModule,w:Widget)=> kvModule.copy(root = Some(w))
    case (kvModule,c:ClassRule) => kvModule.copy(classes = kvModule.classes ++ (c :: Nil))
    case (kvModule,_) => kvModule
  }.copy(rawNodes = lineNodes)
}
