package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source
import scala.util.matching.Regex
class Day16AS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day16.txt"

  override protected val whiteSpace: Regex = "\\s*".r

  override def read(sc: Scanner): Unit = ???
  override def calculate(): String = ???
  override def file(): Path = Paths.get(f)

  //noinspection SourceNotClosed
  override def oneLine(): String =
    ((lines: Array[Array[String]], rangeP: Parser[Int ~ Int]) => {
      ((classes: Map[String, Int => Boolean], yourTicket: List[Int], nearbyTickets: List[List[Int]]) =>
        /* Process input */
        nearbyTickets.flatMap(_.filter(i => classes.values.forall(f => !f(i)))).sum
      ) (
        /* Read input */
        lines.head.map(s => parse(("[a-zA-Z ]+".r <~ ":") ~ (rangeP <~ "or") ~ rangeP, s).get)
          .map({
            case name ~ (r1l ~ r1r) ~ (r2l ~ r2r) => (name, (i: Int) => (r1l <= i && i <= r1r) || (r2l <= i && i <= r2r))
          }).toMap,
        lines.drop(1).head.head.split(",").map(_.toInt).toList,
        lines.drop(2).head.map(_.split(",").map(_.toInt).toList).toList
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines()
        .mkString("\n")
        .replaceAll("your ticket[:]\\s*\n", "")
        .replaceAll("nearby tickets[:]\\s*\n", "")
        .split("\n\n")
        .map(_.split("\n").filterNot(_.isEmpty)),
      (("\\d+".r ^^ { _.toInt }) <~ "-") ~ ("\\d+".r ^^ { _.toInt })
    ) + ""
}

object Day16AS {
  def main(args: Array[String]): Unit = {
    println(new Day16AS().oneLine())
  }
}
