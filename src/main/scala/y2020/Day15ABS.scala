package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source
class Day15ABS extends AdventOfCode{
  private val f = "misc/src/main/resources/day15.txt"

  override def read(sc: Scanner): Unit = ???
  override def calculate(): String = ???
  override def file(): Path = Paths.get(f)

  //noinspection SourceNotClosed
  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: Map[Int, Int]) => {
        /* Process input */
        Iterator.iterate(((0 to 30000000).map(input.getOrElse(_, -1)).toVector, 0, input.size))({
          case (acc, n, i) => (acc.updated(n, i), if (acc(n) == -1) 0 else i - acc(n), i + 1)
        }).find(_._3 == 30000000 - 1)
          .map(_._2).get
      }) (
        /* Read input */
        lines.head.split(",").map(_.toInt).zipWithIndex.toMap
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().filterNot(_.isEmpty).toList
    ) + ""
}

object Day15ABS {
  def main(args: Array[String]): Unit = {
    println(new Day15ABS().oneLine())
  }
}
