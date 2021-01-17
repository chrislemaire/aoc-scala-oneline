package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day1AS extends AdventOfCode {
  var l: List[Long] = Nil

  private final val f = "misc/src/main/resources/day1-a.txt"

  override def read(sc: Scanner): Unit =
    while (sc.hasNext()) l = sc.nextLong() :: l

  override def calculate(): String =
    (for (x <- l;
          y <- l
          if x + y == 2020) yield x * y).head.toString

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: List[Long]) => {
        /* Process input */
        (for (x <- input;
             y <- input
             if x + y == 2020) yield (x * y).toString).headOption.getOrElse("")
      })(
        /* Read input */
        lines.map(_.toLong)
      )
    })(
      /* Read file */
      Source.fromFile(f).getLines().toList.filterNot(_.isEmpty)
    )
}

object Day1AS {
  def main(args: Array[String]): Unit = {
    new Day1AS().applyBoth()
  }
}
