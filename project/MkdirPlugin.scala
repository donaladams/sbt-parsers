import sbt._
import sbt.complete.DefaultParsers._
import sbt.complete.Parser

object MkdirParser {

  /* usage: mkdir [-pv] [-m mode] directory */

  def flagParser(flags: Set[Char]): Parser[Set[Char]] = {
    val singleCharParser: Parser[Char] = flags.map(literal).reduce(_ | _)
    ("-" ~> (singleCharParser+)).map(_.toSet)
  }

//  def disjunctionParser[A, B](as: Seq[A])(p: Parser[B]): Parser[B] = as.map(literal).reduce(_ | _)

  def inSet(chars: Set[Char]): Parser[Char] = chars.map(literal).reduce(_ | _)

  val optionFlags = Set('p', 'v')
  val optionParser: Parser[Set[Char]] = flagParser(optionFlags)

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


  val permissionParser: Parser[Char] = inSet(Set('r', 's', 't', 'w', 'x', 'X', 'u', 'g', 'o'))
  val permissions: Parser[Seq[Char]] = permissionParser+

  val op: Parser[Char] = inSet(Set('+', '-', '='))
  val who: Parser[Char] = inSet(Set('a', 'u', 'g', 'o'))
  val action: Parser[(Char, Seq[Char])] = op ~ permissions
  val clause: Parser[(Seq[Char], Seq[(Char, Seq[Char])])] = (who*) ~ (action+)
  val symbolicModeParser: Parser[Seq[(Seq[Char], Seq[(Char, Seq[Char])])]] = (clause <~ literal(',').?)+

  val octalModeParser: Parser[Int] = {
    val bitParser = inSet(Set('0', '1', '2', '3', '4', '5', '6', '7'))
    val bits: Parser[Seq[Char]] = bitParser+

    def validate(chars: Seq[Char]): Boolean = chars.nonEmpty && chars.size < 5
    bits.filter(validate, s => "is not a valid Octal mode").map(_.mkString.toInt)
  }

  val modeParser: Parser[Any] = ("-m" ~ Space) ~> (octalModeParser | symbolicModeParser)

  val mkdirParser: Parser[Seq[Any]] = {
    val component: Parser[Any] = OptSpace ~> (optionParser | modeParser)
    component*
  }
}


case class MkdirOptions(verbose: Boolean = false,
                        createDirectories: Boolean = false,
                        mode: Option[String] = None,
                        directory: Option[String] = None)

object MkdirOptions {
  case class CmdOpt[T](literal: Char, value: T)
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

