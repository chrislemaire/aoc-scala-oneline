package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day7BS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day7.txt"

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((input: Map[String, List[(String, Int)]]) =>
        /* Process input */
        Iterator
          .iterate(List(("shiny gold", 1)))(_
              .flatMap(bag => input
                .getOrElse(bag._1, List())
                .toMap.mapValues(_ * bag._2).toList)
          )
          .map(_.map(_._2).sum)
          .takeWhile(_ != 0)
          .sum - 1 + ""
        ) (
        /* Read input */
        lines
          .map(line => parse(
            ("\\w+\\s\\w+".r <~ "bags contain") ~
              (rep1sep("\\d+".r.^^(_.toInt) ~ ("\\w+\\s\\w+".r <~ "bag[s]?".r), "[,.]".r) |||
                ("no other bags." ^^ (_ => List[Int ~ String]()))), line))
          .flatMap({
            case Success(id ~ contains, _) => Some(id -> contains.map({ case n ~ idc => idc -> n }))
            case _ => None
          }).toMap
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day7BS {
  def main(args: Array[String]): Unit = {
    println(new Day7BS().oneLine())
  }
}
