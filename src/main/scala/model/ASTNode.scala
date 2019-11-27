package model

import org.bitbucket.inkytonik.kiama.==>
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy

trait ASTNode extends FoldableAST { self =>
  def traverseAndApply(s:Strategy):ASTNode
  def rewrite(fp:ASTNode ==> ASTNode): ASTNode = self.traverseAndApply(rule(fp))
}