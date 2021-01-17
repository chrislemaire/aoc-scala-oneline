package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day12BS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day12.txt"

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((data: List[(Char, Int)]) =>
        /* Process input */
        Some(data.foldLeft(((0, 0), (10, 1)))({
          case (((x, y), (wX, wY)), ('N', n)) => ((x, y), (wX, wY + n))
          case (((x, y), (wX, wY)), ('S', n)) => ((x, y), (wX, wY - n))
          case (((x, y), (wX, wY)), ('E', n)) => ((x, y), (wX + n, wY))
          case (((x, y), (wX, wY)), ('W', n)) => ((x, y), (wX - n, wY))
          case (((x, y), (wX, wY)), ('F', n)) => ((x + n * wX, y + n * wY), (wX, wY))
          case (((x, y), (wX, wY)), ('L', n)) => ((x, y), (0 until n / 90).foldLeft((wX, wY))((w, _) => (-w._2, w._1)))
          case (((x, y), (wX, wY)), ('R', n)) => ((x, y), (0 until n / 90).foldLeft((wX, wY))((w, _) => (w._2, -w._1)))
        })).map(t => Math.abs(t._1._1) + Math.abs(t._1._2)).get
        ) (
        /* Read input */
        lines.map(line => line.charAt(0) -> line.substring(1).toInt)
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day12BS {
  def main(args: Array[String]): Unit = {
    println(new Day12BS().oneLine())
  }
}
