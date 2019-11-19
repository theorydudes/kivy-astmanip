package model

import model.lines.{ClassRuleNode, WidgetNode}

case class KvModule(root:WidgetNode, classes:List[ClassRuleNode])
