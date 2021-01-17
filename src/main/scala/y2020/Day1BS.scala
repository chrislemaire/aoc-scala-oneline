package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day1BS extends AdventOfCode {
  var l: List[Long] = Nil

  private final val f = "misc/src/main/resources/day1-a.txt"

  override def read(sc: Scanner): Unit =
    while (sc.hasNext()) l = sc.nextLong() :: l

  override def calculate(): String =
    (for (x <- l;
          y <- l;
          z <- l
          if x + y + z == 2020) yield x * y * z).head.toString

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: List[Long]) => {
        /* Process input */
        (for (x <- input;
              y <- input;
              z <- input
              if x + y + z == 2020) yield (x * y * z).toString).headOption.getOrElse("")
      }) (
        /* Read input */
        lines.map(_.toLong)
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().toList.filterNot(_.isEmpty)
    )
}

object Day1BS {
  def main(args: Array[String]): Unit = {
    new Day1BS().applyBoth()
  }
}
