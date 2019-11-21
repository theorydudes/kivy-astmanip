package util

import org.bitbucket.inkytonik.kiama.util.StringSource

case class PreProcessor(post:String){
  val parserInput: StringSource = StringSource(post)
}

object PreProcessor {
  def apply(pre: String): PreProcessor = {
    /**
     * Replace all occurences of 4 spaces at the beginning of a line with `\t` and concatenate
     * lines with the `\n` character.
     *
     * @param lines
     * @return
     */
    def tabAndConcat(lines:List[String]): String = lines match {
      case Nil => ""
      case head :: tl =>
        val indexOfFirstNonWhitespace = head.indexWhere(c => """[^\s]""".r.pattern.matcher(c.toString()).matches())
        if(indexOfFirstNonWhitespace != -1) {
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
     * @param lines
     * @return
     */
    def squash(lines:List[String]): String = lines match {
      case Nil => ""
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
