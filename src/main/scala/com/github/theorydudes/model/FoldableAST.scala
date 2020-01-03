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

trait FoldableAST {
  /**
   * Folds `self` bottom up by applying `op`
   * first onto the children of `self` in
   * left to right order and then onto `self`.
   *
   * @param z Starting state.
   * @param op Operation that is applied on the children of `self` and `self`.
   * @tparam B Result type of the fold.
   * @return A folded result of type `B`.
   */
  def foldLeft[B](z:B)(op:(B,ASTNode) => B):B

  /**
   * Checks if `astNode` is a subtree of `self` by iterating through all children of
   * `self` in left to right order bottom up.
   *
   * @param astNode AstNode that is checked
   * @return true, if `astNode` is a subtree of `self`, false otherwise.
   */
  def exists(astNode: ASTNode):Boolean =
    foldLeft(false){case (b,ast) => b || ast.equals(astNode)}

  /**
   * Checks, if `f` holds for all subtrees of `self`.
   *
   * @param f function that contains a condition for all ASTNodes.
   */
  def forall(f: ASTNode => Boolean):Boolean =
    foldLeft(true){case (b,ast) => b && f(ast)}

  /**
   * Folds `self` by applying f on the leftmost leaf of `self`
   * and `g` on all other subtrees wrapping the result in [[scala.Some]].
   *
   * @param f Function that is applied to the leftmost leaf
   * @param g Function that is applied to all subtrees of `self` except the leftmost leaf.
   * @tparam B Result-type of the fold.
   */
  def reduceLeftToOption[B](f:ASTNode => B)(g:(B,ASTNode) => B):Option[B] =
    foldLeft(Option.empty[B]){
      case (Some(b),astNode) => Some(g(b,astNode))
      case (None,astNode) => Some(f(astNode))
    }

  /**
   * Returns the element at position `index` in the
   * order of `self.toList()`
   *
   * @param index Position of the element to get.
   */
  def get(index:Int): ASTNode =
    toList(index)

  /**
   * Returns the index of the first element that is equal to `node`.
   *
   * @param node Node to find index of.
   */
  def indexOf(node:ASTNode):Int =
    toList.indexOf(node)

  /**
   *  Returns the result of `pf` for the first subtree of `self` that is defined
   *  in `pf`. Returns `None` if no subtree is defined in `pf`.
   *
   * @param pf partial function that defines which element to collect
   * @tparam B return type of `pf`
   */
  def collectFirst[B](pf: PartialFunction[ASTNode,B]):Option[B] =
    foldLeft(Option.empty[B]){
      case (None,astNode) =>
        if (pf.isDefinedAt(astNode)) {
          Some(pf.apply(astNode))
        } else {
          None
        }
      case (s@Some(_),_) => s
      case _ => None
    }

  /**
   * Returns the first subtree of `self` that `f(subtree)` returns true for or `None` if
   * no subtree matches.
   *
   * @param f Predicate.
   */
  def find(f:ASTNode => Boolean):Option[ASTNode] =
    foldLeft(Option.empty[ASTNode]){
      case (None,astNode) if f(astNode) => Some(astNode)
      case (s@Some(_),_) => s
      case _ => None
    }

  /**
   * Returns a list representation of `self` by collecting each subtree in bottom up
   * and left to right order.
   * @return
   */
  def toList:List[ASTNode] =
    foldLeft(scala.collection.mutable.ListBuffer.empty[ASTNode]){
      (buf,a) => buf += a
    }.toList

  /**
   * Returns all subtrees of `self` that match the predicate `p`.
   *
   * @param p Predicate.
   */
  def filter(p:ASTNode => Boolean):List[ASTNode] =
    foldLeft(scala.collection.mutable.ListBuffer.empty[ASTNode]){
      (buf,a)  => if (p(a)) buf += a else buf
    }.toList


}
