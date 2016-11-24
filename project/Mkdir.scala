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
  case class AbsoluteMode(mode: String) extends Mode

  case class MkdirCommand(verbose: Boolean = false,
                          createIntermediate: Boolean = false,
                          mode: Option[Mode] = None,
                          directories: Seq[File] = Seq()) {

    def valid(): Boolean = {
      val options = Seq("-m", "-v", "-p")
      directories.map(d => !options.contains(d.getPath)).forall(identity)
    }

  }



  object MkdirCommand {

    def merge(m1: MkdirCommand, m2: MkdirCommand): MkdirCommand = {
      m1.copy(
        createIntermediate = m1.createIntermediate || m2.createIntermediate,
        verbose = m1.verbose || m2.verbose,
        mode = m1.mode.orElse(m2.mode),
        directories = m1.directories ++ m2.directories
      )
    }

  }

  def run(command: MkdirCommand): Unit = {
    println(command)
  }

}
