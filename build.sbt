import Mkdir.MkdirCommand
import MkdirParserSBT._
import MkdirParserScopt.{MkdirConfig, _}
import sbt.complete.DefaultParsers._

name := "sbt-parsers"

version := "1.0"

scalaVersion := "2.12.0"

commands ++= Seq(mkdirCmd, mkdirScoptCmd)

lazy val mkdir = inputKey[Unit]("make directories with sbt parsers")

mkdir := {
  // The ".parsed" macro is available in InputTasks and
  // it applies the given parser the user's command line input
  val mkdirCommand: MkdirCommand = MkdirParserSBT.mkdirParser.parsed
  Mkdir.run(mkdirCommand)
}



def mkdirCmd = Command("mkdirCommand")(_ => mkdirParser) { (state, mkdirCmd: MkdirCommand) =>
  Mkdir.run(mkdirCmd)
  state
}

lazy val mkdirScopt = inputKey[Unit]("make directories with scopt")

mkdirScopt := {
  parser.parse(spaceDelimited("args").parsed, MkdirConfig()) match {
    case Some(c) => println(c)
    case None => ()
  }
}

def mkdirScoptCmd = Command.args("mkdirScoptCommand", parser.usage) { (state, args) =>
  parser.parse(args, MkdirConfig()) match {
    case Some(c) => println(c)
    case None => ()
  }
  state
}
