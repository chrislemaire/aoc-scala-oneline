package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day5BS extends AdventOfCode {
  private final val f = "misc/src/main/resources/day5.txt"

  private var seats: Set[(Int, Int)] = Set()

  override def read(sc: Scanner): Unit =
    seats = Source.fromFile(f).getLines()
      .map(_
        .replaceAll("[BR]", "1")
        .replaceAll("[FL]", "0"))
      .map(s => Integer.parseInt(s.substring(0, 7), 2) -> Integer.parseInt(s.substring(7), 2))
      .toSet

  override def calculate(): String =
    (0 to 127).map(r =>
      s"${r}\t${(0 to 7).map(c => if (seats((r, c))) 'X' else ' ').mkString}").mkString("\n")

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: Set[(Int, Int)]) => {
        /* Process input */
        (0 to 127).map(r =>
          s"${r}\t${(0 to 7).map(c => if (seats((r, c))) 'X' else ' ').mkString}").mkString("\n")
      }) (
        /* Read input */
        Source.fromFile(f).getLines()
          .map(_
            .replaceAll("[BR]", "1")
            .replaceAll("[FL]", "0"))
          .map(s => Integer.parseInt(s.substring(0, 7), 2) -> Integer.parseInt(s.substring(7), 2))
          .toSet
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().mkString("\n")
        .split("\n\n").toList
    ) + ""
}

object Day5BS {
  def main(args: Array[String]): Unit = {
    new Day5BS().applyBoth()
  }
}
