lazy val root = project
    .in(file("."))
    .settings(
        name := "kivy-astmanip",
        scalaVersion := "2.12.8",
        scalacOptions += "-Ypartial-unification",
        organization := "theorydudes",
        version := "0.1",
        libraryDependencies ++=
          Seq (
          "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
          "org.scalactic" %% "scalactic" % "3.0.8",
          "org.scalatest" %% "scalatest" % "3.0.8" % "test",
          "com.github.python3parser" % "python3parser" % "1.0.1"
        )
    )

resolvers += Resolver.bintrayRepo("danielnaczo","Python3Parser")
