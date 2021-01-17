package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day9BS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day9.txt"

  private final val A = 27911108L

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((data: List[Long]) =>
        /* Process input */
        ((prefixes: Map[Long, Int]) =>
          prefixes
            .filterKeys(k => prefixes.contains(k + A))
            .filter(t => t._2 + 1 != prefixes.getOrElse(t._1 + A, 0))
            .map(e => data.slice(e._2 + 1, prefixes(e._1 + A) + 1))
            .map(l => l.min + l.max)
            .head
          )(data.scanLeft(0L)((acc, n) => acc + n).drop(1)
          .zipWithIndex.toMap)
        ) (
        /* Read input */
        lines.map(_.toLong)
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day9BS {
  def main(args: Array[String]): Unit = {
    println(new Day9BS().oneLine())
  }
}

