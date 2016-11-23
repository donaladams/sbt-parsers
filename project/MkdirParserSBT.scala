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

  /**
    * Boolean options
    */
  private val optionFlags = Set('p', 'v')
  private def flag: Parser[Char] = chars(optionFlags.mkString)
  private def flags: Parser[Set[Char]] = {
    val cmdLineFlag: Parser[Seq[Char]] = token(Space ~> literal('-')) ~> token(flag+)

    cmdLineFlag.*.map(_.flatten.toSet)
  }

  /**
    * Symbolic mode parsers
    */
  private val permissionChars = Set('r', 's', 't', 'w', 'x', 'X', 'u', 'g', 'o')
  private val opChars = Set('+', '-', '=')
  private val whoChars = Set('a', 'u', 'g', 'o')
  private val octalChars = Set('0', '1', '2', '3', '4', '5', '6', '7')

  private val permissions: Parser[Seq[Permission]] = chars(permissionChars.mkString).map(Permission)*
  private val op: Parser[Op] = chars(opChars.mkString).map(Op(_))
  private val who: Parser[Who] = chars(whoChars.mkString).map(Who)
  private val action: Parser[Action] = (op ~ permissions).map(Action.tupled)
  private val clause: Parser[Clause] = ((who*) ~ (action+)).map(Clause.tupled)
  private val symbolicMode: Parser[SymbolicMode] = ((clause <~ ','.?)+).map(SymbolicMode)

  /**
    * Absolute mode parsers
    */
  val octalDigit: Parser[Char] = chars(octalChars.mkString) // chars matches any Char in the given string
  val octalNumber: Parser[String] = (octalDigit+).map(_.mkString)
  val absoluteMode: Parser[AbsoluteMode] = octalNumber.map(AbsoluteMode)

  /**
    * Composes an AbsoluteMode parser and a SymbolicMode parser to parse a valid mode from input
    */
  private val modeParser: Parser[Mode] = ("-m" ~ Space) ~> (absoluteMode | symbolicMode)

  /**
    * Directory parsers
    */
  private val directory: Parser[File] = StringBasic.map(p => new File(p))
  private val directories: Parser[Seq[File]] = (Space ~> directory)+


  /**
    * Compose and transform basic parsers to return a MkdirCommand
    */
  private val mkdirMode: Parser[MkdirCommand] = modeParser.map(m => MkdirCommand(mode = Some(m)))

  private val mkdirFlags: Parser[MkdirCommand] = flags.map { f =>
    MkdirCommand(verbose = f.contains('v'), createIntermediate = f.contains('p'))
  }

  private val mkdirOptions: Parser[MkdirCommand] = {
    val mkdirFlat: Parser[Seq[MkdirCommand]] = (OptSpace ~> (mkdirFlags | mkdirMode))*

    mkdirFlat.map(_.foldLeft(MkdirCommand())(MkdirCommand.merge))
  }

  private val mkdirDirectories: Parser[MkdirCommand] = directories.map(ds => MkdirCommand(directories = ds))

  val mkdirParser: Parser[MkdirCommand] = {
    (mkdirOptions ~ mkdirDirectories)
      .map(x => Seq(x._1, x._2))
      .map(_.foldLeft(MkdirCommand())(MkdirCommand.merge))
  }



  
}
