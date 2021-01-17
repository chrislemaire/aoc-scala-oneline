package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day3AS extends AdventOfCode {
  private final val f = "misc/src/main/resources/day3.txt"

  private var trees: List[String] = Nil

  override def read(sc: Scanner): Unit =
    trees = Source.fromFile(f).getLines().toList
      .filterNot(_.isEmpty)

  override def calculate(): String =
    Iterator.iterate((0, 0))(t => (t._1 + 1, (t._2 + 3) % trees(t._1).length))
      .takeWhile(t => t._1 < trees.size)
      .count(t => trees(t._1)(t._2) == '#')
      .toLong + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: (Int, Int, Set[(Int, Int)])) => {
        /* Process input */
        Iterator.iterate((0, 0))(t => (t._1 + 1, (t._2 + 3) % input._2))
          .takeWhile(t => t._1 < input._1)
          .count(input._3)
          .toLong + ""
      }) (
        /* Read input */
        (lines.size, lines.headOption.map(_.length).getOrElse(0),
          lines.zipWithIndex.flatMap(t =>
            t._1.zipWithIndex.filter(_._1 == '#')
              .map(tl => (t._2, tl._2))).toSet)
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().toList
        .filterNot(_.isEmpty)
    ) + ""
}

object Day3AS {
  def main(args: Array[String]): Unit = {
    new Day3AS().applyBoth()
  }
}


