import sbt.complete.DefaultParsers._
import sbt.complete.Parser

object ExamplesParser {

  // Matches "scala"
  val scala: Parser[String] = "scala"

  // Matches "java"
  val java: Parser[String] = "java"

  // Matches "java" or "scala"
  val language: Parser[String] = java | scala

  // Matches a possibly empty comma-separated list of languages.
  // ? means optional match
  // ~> means return the result of the parser to the right - language in this case
  // so we would discard any commas from the results
  val languages: Parser[Seq[String]] = (','.? ~> language)*

  val examplesParser = OptSpace ~> languages
}