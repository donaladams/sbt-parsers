import java.io.File
import sbt.complete.DefaultParsers._
import sbt.complete.Parser





object MkdirParserScopt {

  case class MkdirConfig(createIntermediate: Boolean = false,
                         verbose: Boolean = false,
                         mode: String = "0777",
                         directories: Seq[File] = Seq())


  lazy val parser = new scopt.OptionParser[MkdirConfig]("mkdir") {
    head("mkdir", "make directories")

    opt[Unit]('p', "create").action((_, c) =>
      c.copy(createIntermediate = true)
    ).text("-p is used to create intermediate directories")

    opt[Unit]('v', "verbose").action((_, c) =>
      c.copy(verbose = true)
    ).text("verbose output")

    opt[String]('m', "mode").action((m, c) =>
      c.copy(mode = m)
    ).text("the directory creation mode")

    help("help").text("prints this usage text")

    arg[File]("<dir>...").unbounded().required().action((x, c) =>
      c.copy(directories = c.directories :+ x)
    ).text("directories to create")

    override def terminate(exitState: Either[String, Unit]): Unit = ()
  }

  lazy val mkdirParserScopt: Parser[MkdirConfig] = {

    println("two")

    def runParser(input: Seq[String]): Parser[MkdirConfig] = {
      println("parse!")
      parser.parse(input, MkdirConfig()) match {
        case Some(conf) => Parser.success(conf)
        case None => Parser.failure("Oh No!")
      }
    }

    spaceDelimited("<arg>")
      .map(echo(_, "Input"))
      .flatMap(runParser)
  }

  def echo[T](t: T, prefix: String = ""): T = {
    println(prefix + t)
    t
  }
}