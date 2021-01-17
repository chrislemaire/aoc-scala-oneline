package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day5AS extends AdventOfCode {
  private final val f = "misc/src/main/resources/day5.txt"

  private var seats: List[Int] = Nil

  override def read(sc: Scanner): Unit =
    seats = Source.fromFile(f).getLines()
      .map(_
        .replaceAll("[BR]", "1")
        .replaceAll("[FL]", "0"))
      .map(Integer.parseInt(_, 2))
      .toList

  override def calculate(): String =
    seats.max + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: List[Int]) => {
        /* Process input */
        input.max + ""
      }) (
        /* Read input */
        lines
          .map(_
          .replaceAll("[BR]", "1")
          .replaceAll("[FL]", "0"))
          .map(Integer.parseInt(_, 2))
          .toList
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().mkString("\n")
        .split("\n\n").toList
    ) + ""
}

object Day5AS {
  def main(args: Array[String]): Unit = {
    new Day5AS().applyBoth()
  }
}
