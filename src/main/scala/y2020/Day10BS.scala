package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day10BS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day10.txt"

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
        (0L :: data).foldRight(Map((data.last + 3) -> 1L))((c, acc) =>
          acc + (c -> (acc.getOrElse(c + 1, 0L) + acc.getOrElse(c + 2, 0L) + acc.getOrElse(c + 3, 0L))))(0)
        ) (
        /* Read input */
        lines.map(_.toLong).sorted
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day10BS {
  def main(args: Array[String]): Unit = {
    println(new Day10BS().oneLine())
  }
}

