
name := "sbt-parsers"

version := "1.0"

scalaVersion := "2.12.0"

lazy val echo = inputKey[Unit]("Repeat the input")

lazy val root = (project in file("."))
  .enablePlugins(MkdirPlugin)
  .settings(
    name := "hello",
    version := "1.0",
    scalaVersion := "2.12.0"
  )

