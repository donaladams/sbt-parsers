import java.io.File

import sbt.complete.DefaultParsers._
import sbt.complete.Parser

object MkdirParserSBT {

  /*
  usage: mkdir [-pv] [-m mode] directory

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


  import Mkdir._

  private def flagParser(flags: Set[Char]): Parser[Set[Char]] = {
    val singleCharParser: Parser[Char] = flags.map(literal).reduce(_ | _)
    ("-" ~> (singleCharParser+)).map(_.toSet)
  }

  private def oneOf(chars: Set[Char]): Parser[Char] = chars.map(literal).reduce(_ | _)

  private val mkdirFlagsParser: Parser[Flags] = {
    def updateFlags(flags: Flags, char: Char): Flags = char match {
      case 'p' => flags.copy(createIntermediate = true)
      case 'v' => flags.copy(verbose = true)
    }
    val rawFlags: Parser[Set[Char]] = flagParser(Set('p', 'v'))
    rawFlags.map { flags => flags.foldLeft(Flags())(updateFlags)}
  }

  private val permissionParser: Parser[Char] = oneOf(Set('r', 's', 't', 'w', 'x', 'X', 'u', 'g', 'o'))
  private val permissions: Parser[Seq[Permission]] = permissionParser.map(Permission)*

  private val op: Parser[Op] = oneOf(Set('+', '-', '=')).map(Op(_))
  private val who: Parser[Who] = oneOf(Set('a', 'u', 'g', 'o')).map(Who)
  private val action: Parser[Action] = (op ~ permissions).map(Action.tupled)
  private val clause: Parser[Clause] = ((who*) ~ (action+)).map(Clause.tupled)

  private val symbolicModeParser: Parser[SymbolicMode] = ((clause <~ literal(',').?)+).map(SymbolicMode)

  private val absoluteModeParser: Parser[AbsoluteMode] = {
    val bitParser = oneOf(Set('0', '1', '2', '3', '4', '5', '6', '7'))
    val bits: Parser[Seq[Char]] = bitParser+

    def validate(chars: Seq[Char]): Boolean =
      chars.nonEmpty && chars.size < 5

    bits
      .filter(validate, s => "is not a valid Absolute mode")
      .map(_.mkString.toInt)
      .map(AbsoluteMode)
  }

  private val modeParser: Parser[Mode] = ("-m" ~ Space) ~> (absoluteModeParser | symbolicModeParser)

  private val directoryParser: Parser[File] = (Space ~> StringBasic).map(s => new File(s))

  private val optionsParser: Parser[MkdirOptions] = {
    def buildOptions(options: MkdirOptions, option: Any): MkdirOptions = option match {
      case m: Mode => options.copy(mode = Some(m))
      case f: Flags => options.copy(flags = f)
      case _ => options
    }
    val components: Parser[Seq[Any]] = (Space ~> (mkdirFlagsParser | modeParser))*

    components.map { c => c.foldLeft(MkdirOptions())(buildOptions) }
  }

  val mkdirParser: Parser[MkdirCommand] = {
    (optionsParser ~ (OptSpace ~> directoryParser+)).map(MkdirCommand.tupled)
  }
  
}
