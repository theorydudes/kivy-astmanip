# kivy-astmanip
[![Bintray](https://img.shields.io/bintray/v/tizuck/maven/kivy-astmanip)](https://bintray.com/tizuck/maven/kivy-astmanip)

A parser, AST manipulator and pretty-printer for the language of [Kivy](https://kivy.org) written in Scala.

## Installation
Kivy-astmanip can be directly imported using sbt:
```sbt
resolvers += Resolver.bintrayRepo("tizuck", "maven")

libraryDependencies += "com.github.theorydudes" %% "kivy-astmanip" % "0.2.0"
```
## Kivy
> Open source Python library for rapid development of applications
> that make use of innovative user interfaces, such as multi-touch apps.

Kivy is based on python but specifies its own language to define user interface structure. 
Kivy-Files are parsed and executed by python-code at runtime.

A defnition for the language of Kivy can be found here: [Kivy-Language](https://kivy.org/doc/stable/api-kivy.lang.html) 
## Example

The following section assumes the imports:

```scala
import com.github.theorydudes.util.KivyParser
import com.github.theorydudes.util.KivyParser._
```

Imagine you have the following Kivy-File consisting of a Root-Module Plot and a LineGraph with some properties:

```
Plot:
  LineGraph:
    background_normal: ''
    background_color: [0,0,0,1]
    bg_color: ['Grey','000']
    orientation: 'vertical'
    padding: '5dp'
    spacing: '5dp'
    mode: "Line"
    id: line_graph
```

Furthermore in this scenario the color of the LineGraph is dependent on some server-response and you need a way to change the `bg_color`
attribute according to this response in Scala. This is where kivy-astmanip comes in handy:

```scala
val serverResponse = "red"

val parser = KivyParser("path/to/file.kv")
val parseResult = parser.root

if(parseResult.isSuccess){
  val manipulatedRoot = serverResponse match {
    case "red" =>
      parseResult
        .get
        .rewrite({case Python("['Grey','000']") => Python("['Red','000']")})
  }
  println(KivyPrettyPrinter.format(manipulatedRoot).layout)
}
 
```
Here, the original Plot defined in the Kivy-file from above is parsed and the AST is then manipulated using the `rewrite` function.

The `KivyPrettyPrinter` prints the AST:

```
Plot:
  LineGraph:
    background_normal: ''
    background_color: [0,0,0,1]
    bg_color: ['Red','000']
    orientation: 'vertical'
    padding: '5dp'
    spacing: '5dp'
    mode: "Line"
    id: line_graph
```

## Future plans
1. Providing a full documentation of model, parser and pretty printer
2. Java-Interface 

## Inspiration
The grammar used to parse kivy-files is based on this [ANTLR3-grammar](https://github.com/kived/kvlang/blob/master/g/kv.g).




