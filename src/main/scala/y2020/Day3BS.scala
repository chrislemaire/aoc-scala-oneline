package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day3BS extends AdventOfCode {
  private final val f = "misc/src/main/resources/day3.txt"

  private var trees: List[String] = Nil

  override def read(sc: Scanner): Unit =
    trees = Source.fromFile(f).getLines().toList
      .filterNot(_.isEmpty)

  override def calculate(): String =
    List((1, 1), (1, 3), (1, 5), (1, 7), (2, 1)).map(slope => {
      Iterator.iterate((0, 0))(t => (t._1 + slope._1, (t._2 + slope._2) % trees(t._1).length))
        .takeWhile(t => t._1 < trees.size)
        .count(t => trees(t._1)(t._2) == '#')
        .toLong
    }).product + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: (Int, Int, Set[(Int, Int)])) => {
        /* Process input */
        List((1, 1), (1, 3), (1, 5), (1, 7), (2, 1)).map(slope => {
          Iterator.iterate((0, 0))(t => (t._1 + slope._1, (t._2 + slope._2) % input._2))
            .takeWhile(t => t._1 < input._1)
            .count(input._3)
            .toLong
        }).product
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

object Day3BS {
  def main(args: Array[String]): Unit = {
    new Day3BS().applyBoth()
  }
}
