import sbt._
import sbt.complete.DefaultParsers._
import Keys._

object MkdirPlugin extends AutoPlugin {

  import Mkdir._
  import MkdirParserSBT._
  import MkdirParserScopt._

  override def trigger = noTrigger
  override lazy val projectSettings = Seq(

    mkdir := {
      val command: MkdirCommand = mkdirParser.parsed
      println(command)
    },

    mkdirScopt := {
      parser.parse(args.evaluated, MkdirConfig()) match {
        case Some(c) => println(c)
        case None => ()
      }
    },

    args := { spaceDelimited("").parsed },

    commands ++= Seq(mkdirSbtCmd, mkdirScoptCmd, examples, mkdirSbtFlatCmd)
  )

  lazy val args = inputKey[Seq[String]]("Retrieve the user's args")
  lazy val mkdir = inputKey[Unit]("make directories with sbt parsers")
  lazy val mkdirScopt = inputKey[Unit]("make directories with scopt")

  def mkdirSbtCmd = Command("mkdirSbtCommand")(_ => mkdirParser) { (state, mkdirCmd: MkdirCommand) =>
    println(mkdirCmd)
    state
  }

  def mkdirSbtFlatCmd = Command("mkdirSbtFlatCommand")(_ => mkdirParser) { (state, m: MkdirCommand) =>
    println(m)
    state
  }

  def mkdirScoptCmd = Command.args("mkdirScoptCommand", parser.usage) { (state, args) =>
    parser.parse(args, MkdirConfig()) match {
      case Some(c) => println(c)
      case None => ()
    }
    state
  }

  def examples = Command("examples")(_ => ExamplesParser.examplesParser) { (state, parsed) =>
    println(parsed)
    state
  }


}

