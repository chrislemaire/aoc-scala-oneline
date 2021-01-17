package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day12AS extends AdventOfCode with RegexParsers {
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
      ((data: List[(Char, Int)], move: List[(Int, Int)]) =>
        /* Process input */
        Some(data.foldLeft(((0, 0), (1, 0)))({
          case (((x, y), (dirX, dirY)), ('N', n)) => ((x, y + n), (dirX, dirY))
          case (((x, y), (dirX, dirY)), ('S', n)) => ((x, y - n), (dirX, dirY))
          case (((x, y), (dirX, dirY)), ('E', n)) => ((x + n, y), (dirX, dirY))
          case (((x, y), (dirX, dirY)), ('W', n)) => ((x - n, y), (dirX, dirY))
          case (((x, y), dir), ('L', n)) => ((x, y), move((move.indexOf(dir) + n / 90) % 4))
          case (((x, y), dir), ('R', n)) => ((x, y), move(((move.indexOf(dir) - n / 90) % 4 + 4) % 4))
          case (((x, y), (dirX, dirY)), ('F', n)) => ((x + dirX * n, y + dirY * n), (dirX, dirY))
        })).map(t => Math.abs(t._1._1) + Math.abs(t._1._2)).get
        ) (
        /* Read input */
        lines.map(line => line.charAt(0) -> line.substring(1).toInt),
        List((1, 0), (0, 1), (-1, 0), (0, -1))
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day12AS {
  def main(args: Array[String]): Unit = {
    println(new Day12AS().oneLine())
  }
}

