package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex

class Day16BS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day16.txt"

  override protected val whiteSpace: Regex = "\\s*".r

  override def read(sc: Scanner): Unit = ???
  override def calculate(): String = ???
  override def file(): Path = Paths.get(f)

  //noinspection SourceNotClosed
  override def oneLine(): String =
    ((lines: Array[Array[String]], rangeP: Parser[Int ~ Int]) => {
      ((classes: Map[String, Int => Boolean], yourTicket: List[Long], nearbyTickets: List[List[Int]]) =>
        /* Process input */
        ((validTickets: List[List[Int]]) =>
          Iterator.continually(Iterator.iterate(
            ("", 1L, classes.mapValues(f => validTickets.flatMap(_.zipWithIndex.filter(t => f(t._1)).map(_._2))
              .groupBy(identity).mapValues(v => v.size).filter(_._2 == validTickets.size).keySet)))(t =>
            t._3.toList.sortBy(_._2.size).headOption
              .map(t => Random.nextInt(t._2.size) -> t)
              .map(t => (t._2._1, t._2._2.drop(t._1).head))
              .map(e => (e._1, if (e._1.startsWith("departure")) yourTicket(e._2) else 1,
                t._3.mapValues(_.filter(_ != e._2)).filterNot(_._2.isEmpty)))
              .getOrElse(("", 1, Map())))
            .takeWhile(_._3.nonEmpty)
            .toList)
            .find(_.size == classes.size)
            .map(_.map(_._2).product).get
          ) (nearbyTickets.filterNot(_.exists(i => classes.values.forall(f => !f(i)))))
        ) (
        /* Read input */
        lines.head.map(s => parse(("[a-zA-Z ]+".r <~ ":") ~ (rangeP <~ "or") ~ rangeP, s).get)
          .map({
            case name ~ (r1l ~ r1r) ~ (r2l ~ r2r) => (name, (i: Int) => (r1l <= i && i <= r1r) || (r2l <= i && i <= r2r))
          }).toMap,
        lines.drop(1).head.head.split(",").map(_.toLong).toList,
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
      (("\\d+".r ^^ {
        _.toInt
      }) <~ "-") ~ ("\\d+".r ^^ {
        _.toInt
      })
    ) + ""
}

object Day16BS {
  def main(args: Array[String]): Unit = {
    println(new Day16BS().oneLine())
  }
}
