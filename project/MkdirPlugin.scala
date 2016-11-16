import sbt._
import sbt.complete.DefaultParsers._
import sbt.complete.Parser
import java.io.File

import Mkdir.{Flags, MkdirOptions, Mode}

object MkdirParser {


  import Mkdir._

  /* usage: mkdir [-pv] [-m mode] directory */

  def flagParser(flags: Set[Char]): Parser[Set[Char]] = {
    val singleCharParser: Parser[Char] = flags.map(literal).reduce(_ | _)
    ("-" ~> (singleCharParser+)).map(_.toSet)
  }

  def oneOf(chars: Set[Char]): Parser[Char] = chars.map(literal).reduce(_ | _)

  val mkdirFlagsParser: Parser[Flags] = {
    def updateFlags(flags: Flags, char: Char): Flags = char match {
      case 'p' => flags.copy(createIntermediate = true)
      case 'v' => flags.copy(verbose = true)
    }
    val rawFlags: Parser[Set[Char]] = flagParser(Set('p', 'v'))
    rawFlags.map { flags => flags.foldLeft(Flags())(updateFlags)}
  }

  /*

  mode         ::= clause [, clause ...]
  clause       ::= [who ...] [action ...] action
  action       ::= op [perm ...]
  who          ::= a | u | g | o
  op           ::= + | - | =
  perm         ::= r | s | t | w | x | X | u | g | o

  Examples:
  644           make a file readable by anyone and writable by the owner only.

  go-w          deny write permission to group and others.

  =rw,+X        set the read and write permissions to the usual defaults, but retain any execute permissions that are currently set.

  +X            make a directory or file searchable/executable by everyone if it is already searchable/executable by anyone.

  755
  u=rwx,go=rx
  u=rwx,go=u-w  make a file readable/executable by everyone and writable by the owner only.

  go=           clear all mode bits for group and others.

  g=u-w         set the group bits equal to the user bits, but clear the group write bit.


  */

  val permissionParser: Parser[Char] = oneOf(Set('r', 's', 't', 'w', 'x', 'X', 'u', 'g', 'o'))
  val permissions: Parser[Seq[Permission]] = permissionParser.map(Permission)*

  val op: Parser[Op] = oneOf(Set('+', '-', '=')).map(Op(_))
  val who: Parser[Who] = oneOf(Set('a', 'u', 'g', 'o')).map(Who)
  val action: Parser[Action] = (op ~ permissions).map(Action.tupled)
  val clause: Parser[Clause] = ((who*) ~ (action+)).map(Clause.tupled)

  val symbolicModeParser: Parser[SymbolicMode] = ((clause <~ literal(',').?)+).map(SymbolicMode)

  val absoluteModeParser: Parser[AbsoluteMode] = {
    val bitParser = oneOf(Set('0', '1', '2', '3', '4', '5', '6', '7'))
    val bits: Parser[Seq[Char]] = bitParser+

    def validate(chars: Seq[Char]): Boolean =
      chars.nonEmpty && chars.size < 5

    bits
      .filter(validate, s => "is not a valid Absolute mode")
      .map(_.mkString.toInt)
      .map(AbsoluteMode)
  }

  val modeParser: Parser[Mode] = ("-m" ~ Space) ~> (absoluteModeParser | symbolicModeParser)

  val directoryParser: Parser[File] = (Space ~> StringBasic).map(s => new File(s))

  val optionsParser: Parser[MkdirOptions] = {
    def buildOptions(options: MkdirOptions, option: Any): MkdirOptions = option match {
      case m: Mode => options.copy(mode = Some(m))
      case f: Flags => options.copy(flags = f)
      case _ => options
    }
    val components: Parser[Seq[Any]] = (Space ~> (mkdirFlagsParser | modeParser))*
    
    components.map { c => c.foldLeft(MkdirOptions())(buildOptions) }
  }

  val mkdirParser: Parser[MkdirCommand] = { (optionsParser ~ directoryParser).map(MkdirCommand.tupled) }
}



object Mkdir {

  sealed trait Op {
    def symbol: Char
  }
  object Op {
    def apply(char: Char): Op = char match {
      case Plus.symbol => Plus
      case Minus.symbol => Minus
      case Equals.symbol => Equals
    }
  }
  case object Plus extends Op {
    override val symbol = '+'
  }
  case object Minus extends Op {
    override val symbol = '-'
  }
  case object Equals extends Op {
    override val symbol = '='
  }

  case class Who(char: Char)
  case class Permission(char: Char)
  case class Action(op: Op, permissions: Seq[Permission])
  case class Clause(who: Seq[Who], actions: Seq[Action]) {
    require(actions.nonEmpty)
  }

  sealed trait Mode
  case class SymbolicMode(clauses: Seq[Clause]) extends Mode
  case class AbsoluteMode(mode: Int) extends Mode

  case class Flags(verbose: Boolean = false, createIntermediate: Boolean = false)

  case class MkdirOptions(flags: Flags = Flags(), mode: Option[Mode] = None)

  case class MkdirCommand(options: MkdirOptions, directory: File)

}

object MkdirPlugin extends AutoPlugin {

  import MkdirParser._
  override def trigger = noTrigger
  override lazy val projectSettings = Seq(

    mkdir := {
      val input = mkdirParser.parsed
      println(input)
    }
  )

  lazy val mkdir = inputKey[Unit]("make directories")
}

