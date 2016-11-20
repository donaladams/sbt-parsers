import java.io.File

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

  case class MkdirCommand(options: MkdirOptions, directories: Seq[File])

}
