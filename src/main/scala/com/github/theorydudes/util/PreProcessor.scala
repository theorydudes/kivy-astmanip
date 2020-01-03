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

import org.bitbucket.inkytonik.kiama.util.StringSource

/**
 * Should be instantiated using the `apply`-method in the companion-object
 * which accepts arbitrarily formatted Kivy-Files and converts them in post
 * so it can be consumed by [[KivyParser]].
 *
 * @param post formatted Kivy-file which can be parsed by [[KivyParser]].
 */
case class PreProcessor(post:String){
  val parserInput: StringSource = StringSource(post)
}

object PreProcessor {
  /**
   * Converts arbitrarily formatted Kivy-Files into the format [[KivyParser]] expects stored
   * in a `PreProcessor` instance.
   *
   * Before pre-processing Kivy-files can contain the following formatting properties:
   *  - mixed indentation symbols tabs and spaces.
   *  - empty lines.
   *  - operating system dependent newline characters.
   *
   * Applies formatting changes to `pre` in a way that:
   *   - only tabs are used for indentation.
   *   - no empty lines exist.
   *   - `\n` is solely used for newlines.
   *
   * @param pre Arbitrarily formatted Kivy-File.
   * @return `PreProcessor` with the applied changes on `pre` stored in `PreProcessor.post`.
   */
  def apply(pre: String): PreProcessor = {

    if(pre equals ""){
      throw new IllegalArgumentException("Empty file.")
    }
    /**
     * Replace all occurrences of 4 spaces at the beginning of a line with `\t` and concatenate
     * lines with the `\n` character.
     *
     * @param lines raw lines possibly containing a mixture of `\t` and spaces for indentation.
     * @return kivy-code that only contains `\t`-characters for indentation.
     */
    def tabAndConcat(lines:List[String]): String = lines match {
      case Nil => ""
      case head :: tl =>
        val indexOfFirstNonWhitespace = head.indexWhere(c => """[^\s]""".r.pattern.matcher(c.toString).matches())
        if(indexOfFirstNonWhitespace > 0) {
          val substitutedWhitespaces = head.substring(0, indexOfFirstNonWhitespace).replaceAll(" {4}", "\t")
          val withoutIndentation = head.substring(indexOfFirstNonWhitespace)
          val withoutIndentationSubstituted = withoutIndentation.replaceAll(" {4}", "\t")
          s"$substitutedWhitespaces$withoutIndentationSubstituted\n${tabAndConcat(tl)}"
        } else {
          head + "\n" + tabAndConcat(tl)
        }
    }

    /**
     * eliminates all whitespaces-only lines except for the last `\n` line.
     *
     * @param lines list of lines of kivy-code possibly containing empty lines
     * @return kivy-code that does not contain any empty lines.
     */
    def squash(lines:List[String]): String = lines match {
      case ::(head,Nil) => head
      case ::(head, tl) =>
        val reg = """\s+""".r
        //If a line only consists of whitespace characters
        if(reg.pattern.matcher(head).matches() || head == ""){
          squash(tl)
        } else {
          head + "\n" + squash(tl)
        }
    }

    val lines = pre.split("\n")
    val linesList = lines.toList
    val squashed = squash(linesList).split("\n")
    val operatingSystemIndependentNewlines = squashed.map(s => s.replaceAll("\\r",""))
    new PreProcessor(tabAndConcat(operatingSystemIndependentNewlines.toList))
  }
}
