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

package com.github.theorydudes.util

import com.github.theorydudes.model.ASTNode
import com.github.theorydudes.model.lines._
import org.bitbucket.inkytonik.kiama.parsing
import org.bitbucket.inkytonik.kiama.parsing._
import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, Source, StringSource}
import org.bitbucket.inkytonik.kiama.parsing.Parsers

case class KivyParser(source:Source) {

  case class KivyParserResult[T <: ASTNode](parseResult: ParseResult[T]){
    def isSuccess: Boolean = parseResult match {
      case parsing.Success(_,_) => true
      case _ => false
    }

    def isFailure: Boolean = parseResult match {
      case parsing.Success(_,_) => false
      case _ => true
    }

    def get: T = parseResult match {
      case parsing.Success(result, _) => result
      case _: NoSuccess =>throw new IllegalStateException("No parsing result.")
    }

    def pretty:String = parseResult match {
      case parsing.Success(result, _) =>
        val doc = KivyPrettyPrinter.format(result)
        doc.layout
      case parsing.NoSuccess(_,_) =>
        throw new IllegalStateException("A non successful parsing result may not be pretty printed.")
    }
  }

  lazy val parserRules = new Rules(new Positions)

  private def parseWithRule[T <: ASTNode](parser:parserRules.Parser[T]): KivyParserResult[T] = {
    val preProcessor = PreProcessor(source.content)
    val parsedInput = parserRules.parseAll(parser,preProcessor.parserInput)
    KivyParserResult(parsedInput)
  }

  def topLevel : KivyParserResult[TopLevel] = parseWithRule(parserRules.kv)


  def comment : KivyParserResult[Comment] = parseWithRule(parserRules.comment)


  def directive : KivyParserResult[Directive] = parseWithRule(parserRules.directive)


  def root : KivyParserResult[Root] = parseWithRule(parserRules.root)

  def classRule : KivyParserResult[ClassRule] = parseWithRule(parserRules.classP)

  def template : KivyParserResult[Template] = parseWithRule(parserRules.template)

  def widget : KivyParserResult[Widget] = parseWithRule(parserRules.widget(0))

  def canvas : KivyParserResult[Canvas] = parseWithRule(parserRules.canvas(0))

  def prop : KivyParserResult[Property] = parseWithRule(parserRules.prop(0))

  def instruction : KivyParserResult[Instruction] = parseWithRule(parserRules.instruction(0))

}

object KivyParser {
  case class Path(p:String)
  case class File(f:String)
  def apply(path:Path):KivyParser = KivyParser(FileSource(path.p))
  def apply(file:File):KivyParser = KivyParser(StringSource(file.f))
}
