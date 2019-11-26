name := "scala-kivyparse"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++=
  Seq (
    "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
    "org.scalactic" %% "scalactic" % "3.0.8",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    "com.github.python3parser" % "python3parser" % "1.0.1",
//    "org.typelevel" %% "cats-core" % "2.0.0",
//      "org.typelevel" %% "kittens" % "2.0.0"
)

resolvers += Resolver.bintrayRepo("danielnaczo","Python3Parser")
