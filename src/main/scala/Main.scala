import util.KivyParser.{File, Path}
import util.KivyParser


object Main extends App {
  val ret = KivyParser(Path("src/main/scala/gui.kv")).parse
  if(ret.isSuccess) {
    println(ret)
    println(ret.pretty)
  } else {
    println()
  }
}
