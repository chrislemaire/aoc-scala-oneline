package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day6BS extends AdventOfCode {
  private final val f = "misc/src/main/resources/day6.txt"

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    answers = Source.fromFile(f).getLines()
      .mkString("\n").split("\n\n")
      .map(_.split("\n").map(_.toCharArray.toSet).toList)
      .toList

  override def calculate(): String =
    answers.map(_.reduce(_.intersect(_)).size).sum + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: List[List[Set[Char]]]) =>
        /* Process input */
        answers.map(_.reduce(_.intersect(_)).size).sum + ""
      ) (
        /* Read input */
        lines
          .map(_.split("\n").map(_.toCharArray.toSet).toList)
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().mkString("\n")
        .split("\n\n").toList
    ) + ""
}

object Day6BS {
  def main(args: Array[String]): Unit = {
    new Day6BS().applyBoth()
  }
}
