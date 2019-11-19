name := "scala-kivyparse"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++=
  Seq (
    "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
      "com.github.python3parser" % "python3parser" % "1.0.1"
)

resolvers += Resolver.bintrayRepo("danielnaczo","Python3Parser")
