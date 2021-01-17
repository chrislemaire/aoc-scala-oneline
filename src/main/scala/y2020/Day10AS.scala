package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day10AS extends AdventOfCode with RegexParsers {
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
        Some(data.sorted.zip(0L :: data.sorted).map(t => t._1 - t._2))
          .map(diff => diff.count(_ == 1L) * (diff.count(_ == 3L) + 1))
          .get
        ) (
        /* Read input */
        lines.map(_.toLong)
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day10AS {
  def main(args: Array[String]): Unit = {
    println(new Day10AS().oneLine())
  }
}

