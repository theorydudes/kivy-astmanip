import sbt.Def
import sbt.Keys.developers

lazy val bintraySettings = Seq(
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val root = project
  .in(file("."))
  .settings(bintraySettings)
  .settings(
    name := "kivy-astmanip",
    scalaVersion := "2.12.8",
    scalacOptions += "-Ypartial-unification",
    organization := "com.github.theorydudes",
    version := "0.1.0",
    libraryDependencies ++=
      Seq (
      "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
      "org.scalactic" %% "scalactic" % "3.0.8",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test",
      "com.github.python3parser" % "python3parser" % "1.0.1"
    ),
    developers := List(
      Developer("tizuck","Tilman Zuckmantel","tilman.zuckmantel@udo.edu",url("https://github.com/tizuck"))
    )
  )

resolvers += Resolver.bintrayRepo("danielnaczo","Python3Parser")
