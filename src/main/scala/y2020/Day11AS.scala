package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day11AS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day11.txt"

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((data: Map[(Int, Int), Char]) =>
        ((findNeighbours: (Int, Int, Map[(Int, Int), Char]) => List[Char]) =>
          /* Process input */
          Iterator.iterate((Map((-1, -1) -> 'L'), data))(d =>
            (d._2, d._2.map({
              case ((r, c), 'L')
                if !findNeighbours(r, c, d._2).contains('#') => (r, c) -> '#'
              case ((r, c), '#')
                if findNeighbours(r, c, d._2).count(_ == '#') >= 4 => (r, c) -> 'L'
              case ((r, c), s) => (r, c) -> s
            })))
            .find(d => d._1 == d._2)
            .map(d => d._1.count(_._2 == '#'))
            .get
          ) ((r, c, d) =>
          List(-1, 0, 1)
            .flatMap(x => List(-1, 0, 1)
              .map(y => (r + x, c + y)))
            .filter(_ != (r, c))
            .map(d.getOrElse(_, '.')))
        ) (
        /* Read input */
        lines.zipWithIndex
          .flatMap(r => r._1.zipWithIndex.filter(c => c._1 == 'L')
            .map(c => (r._2, c._2) -> c._1))
          .toMap
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day11AS {
  def main(args: Array[String]): Unit = {
    println(new Day11AS().oneLine())
  }
}

