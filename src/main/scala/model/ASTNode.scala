package model

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy

trait ASTNode extends FoldableAST { self =>
  def rewrite(s:Strategy) =
    org.bitbucket.inkytonik.kiama.rewriting.Rewriter.rewrite()
}