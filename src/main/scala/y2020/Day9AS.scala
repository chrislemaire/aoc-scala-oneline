package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day9AS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day9.txt"

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
        (0 until data.size - 25)
          .map(i => (data.drop(i + 25).head, data.slice(i, i + 25)))
          .find(t => t._2.forall(x => t._2.forall(y => x + y != t._1)))
          .map(_._1).get
        ) (
        /* Read input */
        lines.map(_.toLong)
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day9AS {
  def main(args: Array[String]): Unit = {
    println(new Day9AS().oneLine())
  }
}

