/*
 * Copyright 2019 com.github.theorydudes
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.theorydudes.model

import com.github.theorydudes.util.KivyPrettyPrinter
import org.bitbucket.inkytonik.kiama.==>
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy

/**
 * Base Type for all nodes of a Kivy-AST
 */
trait ASTNode extends FoldableAST { self =>
  /**
   * Traverses the ASTNode and applies Strategy `s` onto `self` and all children of self.
   *
   * `s` is hereby applied bottom up in left to right order.
   *
   * @see [[https://bitbucket.org/inkytonik/kiama/src/032630fa21ddad5cf33cbd6ef9c2f0278661a675/wiki/Rewriting.md]]
   * @param s strategy that is applied to `self` and all children.
   * @return a rewritten ASTNode according to the strategy `s`
   */
  private[theorydudes] def traverseAndApply(s:Strategy):ASTNode

  /**
   * Rewrite the ASTNode `self` by the specification of a partial function `fp`.
   *
   * If we want to change a specific [[model.Python]]-node in the AST for example we could
   * apply the following rewrite strategy:
   *{{{
   *   ast.rewrite({
   *    case Python("[1,2,3]") => Python("[1,2,3,4]")
   *   })
   *}}}
   *
   * Please note, that ASTNodes can not be rewritten arbitrarily. Since each ASTNode implies
   * a specific parameter list. An AST has to stay structure-consistent after applying rewriting rules.
   * A rewriting rule as:
   * {{{
   *   {
   *    case Python(s) => TopLevel(Nil)
   *   }
   * }}}
   * is not valid as a [[model.TopLevel]]-node can not occur at positions where a [[model.Python]]-node can.
   *
   * @see [[https://bitbucket.org/inkytonik/kiama/src/032630fa21ddad5cf33cbd6ef9c2f0278661a675/wiki/Rewriting.md]]
   * @param fp Partial function that defines how the ast should be rewritten.
   * @return A rewritten AST according to the specification in `fp` or the same ast if `fp` could not be applied.
   */
  def rewrite(fp:ASTNode ==> ASTNode): ASTNode = self.traverseAndApply(rule(fp))

  /**
   * Transforms `self` into a well formatted kivy program that can be written
   * into a file.
   *
   * The following ASTNode for example:
   * {{{
   *   TopLevel(
   *    List(
   *      Root(
   *        Widget(
   *          Plot,
   *          List(
   *            Widget(
   *              LineGraph,
   *              List(
   *                Property(background_normal,List('')),
   *                Property(background_color,List([0,0,0,1]))
   *   )))))))
   * }}}
   *
   * is printed:
   * {{{
   * Plot:
   *  LineGraph:
   *    background_normal: ''
   *    background_color: [0,0,0,1]
   * }}}
   *
   * @return A formatted ASTNode that can be interpreted as a Kivy file.
   */
  def pretty:String = KivyPrettyPrinter.format(self).layout
}