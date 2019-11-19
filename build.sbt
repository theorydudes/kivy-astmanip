name := "scala-kivyparse"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++=
  Seq (
    "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
      "org.scalactic" %% "scalactic" % "3.0.8",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
