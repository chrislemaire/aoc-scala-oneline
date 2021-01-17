package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source
import scala.util.matching.Regex

class Day17BS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day17.txt"

  override protected val whiteSpace: Regex = "\\s*".r

  override def read(sc: Scanner): Unit = ???
  override def calculate(): String = ???
  override def file(): Path = Paths.get(f)

  //noinspection SourceNotClosed
  override def oneLine(): String =
    ((lines: List[String]) => {
      ((s: Set[(Int, Int, Int, Int)], neighbours: (Int, Int, Int, Int) => List[(Int, Int, Int, Int)]) =>
        /* Process input */
        (1 to 6).foldLeft(s)((s, _) =>
          for ((x, y, z, w) <- s.flatMap(neighbours.tupled);
               count <- Some(neighbours(x, y, z, w).count(s))
               if (s((x, y, z, w)) && (count == 2 || count == 3)) || (!s((x, y, z, w)) && count == 3))
            yield (x, y, z, w)).size
        ) (
        /* Read input */
        lines.zipWithIndex
          .flatMap(line => line._1.zipWithIndex
            .flatMap(col => if (col._1 == '#') Some((col._2, line._2, 0, 0)) else None))
          .toSet,
        (x, y, z, w) =>
          (for (xd <- -1 to 1;
                yd <- -1 to 1;
                zd <- -1 to 1;
                wd <- -1 to 1
                if !(xd == 0 && yd == 0 && zd == 0 && wd == 0)) yield (x + xd, y + yd, z + zd, w + wd)).toList
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().toList
        .filterNot(_.isEmpty)
    ) + ""
}

object Day17BS {
  def main(args: Array[String]): Unit = {
    println(new Day17BS().oneLine())
  }
}
