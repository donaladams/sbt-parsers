
name := "sbt-parsers"

version := "1.0"

scalaVersion := "2.12.0"

lazy val root = (project in file("."))
  .enablePlugins(MkdirPlugin)
  .settings(
    name := "sbt-parsers",
    version := "1.0",
    scalaVersion := "2.12.0"
  )

