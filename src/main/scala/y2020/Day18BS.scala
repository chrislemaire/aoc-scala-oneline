package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source
import scala.util.matching.Regex

class Day18BS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day18.txt"

  override protected val whiteSpace: Regex = "\\s*".r

  private var lines: List[String] = Nil

  private def num: Parser[Long] = "\\d+".r ^^ { _.toLong }
  private def lExpr: Parser[Long] = num ||| "(" ~> expr <~ ")"

  private def sum: Parser[Long] = lExpr ||| (lExpr <~ "+") ~ sum ^^ { case l ~ r => l + r }
  private def prod: Parser[Long] = sum ||| (sum <~ "*") ~ prod ^^ { case l ~ r => l * r }

  private def expr: Parser[Long] = lExpr ||| prod

  override def read(sc: Scanner): Unit = {
    while (sc.hasNextLine) {
      lines ::= sc.nextLine()
    }
  }
  override def calculate(): String = {
    lines.map(s => parse(expr, s))
      .map(_.get).sum + ""
  }
  override def file(): Path = Paths.get(f)

  //noinspection SourceNotClosed
  override def oneLine(): String =
    ((lines: List[String]) => {
      ((i: List[List[String]]) =>
        /* Process input */
        i
        ) (
        /* Read input */
        lines.map(_.replaceAll("[(]", "( ")
          .replaceAll("[)]", " )")
          .split(" ").toList)
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().toList
        .filterNot(_.isEmpty)
    ) + ""
}

object Day18BS {
  def main(args: Array[String]): Unit = {
    new Day18BS().apply()
  }
}

