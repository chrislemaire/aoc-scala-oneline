package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day11BS extends AdventOfCode with RegexParsers {
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
      ((data: Map[(Int, Int), Char], w: Int, h: Int) =>
        ((neighbours: Map[(Int, Int), List[(Int, Int)]]) =>
          /* Process input */
          Iterator.iterate((Map((-1, -1) -> 'L'), data))(d =>
            (d._2, d._2.map({
              case ((x, y), 'L')
                if !neighbours.getOrElse((x, y), Nil)
                  .map(d._2.getOrElse(_, '.'))
                  .contains('#') => (x, y) -> '#'
              case ((r, c), '#')
                if neighbours
                  .getOrElse((r, c), Nil)
                  .map(d._2.getOrElse(_, '.'))
                  .count(_ == '#') >= 5 => (r, c) -> 'L'
              case ((r, c), s) => (r, c) -> s
            })))
            .find(d => d._1 == d._2)
            .map(d => d._1.count(_._2 == '#'))
            .get
          ) (
          data.keys.map({
            case (x, y) =>
              (x, y) -> List(-1, 0, 1)
                .flatMap(offsetX => List(-1, 0, 1)
                  .map(offsetY => (offsetX, offsetY))
                  .filter(_ != (0, 0)))
                .flatMap(offset => Iterator
                  .iterate((x + offset._1, y + offset._2))(t => (t._1 + offset._1, t._2 + offset._2))
                  .takeWhile(p => 0 <= p._1 && p._1 < w && 0 <= p._2 && p._2 < h)
                  .find(data.contains))
          }).toMap)
        ) (
        /* Read input */
        lines.zipWithIndex
          .flatMap(y => y._1.zipWithIndex
            .filter(x => x._1 == 'L')
            .map(x => (x._2, y._2) -> x._1))
          .toMap,
        lines.headOption.map(_.length).getOrElse(0),
        lines.size
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day11BS {
  def main(args: Array[String]): Unit = {
    println(new Day11BS().oneLine())
  }
}

