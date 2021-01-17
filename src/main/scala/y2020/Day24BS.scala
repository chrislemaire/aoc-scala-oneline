package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day24BS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day24.txt"

  sealed abstract class Dir(val dx: Int, val dy: Int)
  object E extends Dir(2, 0)
  object W extends Dir(-2, 0)
  object NW extends Dir(-1, 1)
  object NE extends Dir(1, 1)
  object SW extends Dir(-1, -1)
  object SE extends Dir(1, -1)

  case class Move(l: List[Dir]) {
    def calcMove: (Int, Int) =
      l.foldLeft((0, 0))((acc, dir) => (acc._1 + dir.dx, acc._2 + dir.dy))
  }

  class MarkSet() {
    var marked: Set[(Int, Int)] = Set()

    def toggle(loc: (Int, Int)): Unit =
      if (marked(loc)) marked -= loc
      else marked += loc

    def count(loc: (Int, Int)): Int =
      List((-1, -1), (1, -1), (1, 1), (-1, 1), (-2, 0), (2, 0))
        .map(t => (t._1 + loc._1, t._2 + loc._2))
        .count(marked)
  }

  private var moves: List[Move] = Nil

  override def read(sc: Scanner): Unit = {
    moves = Source.fromFile(f).getLines()
      .map(parseAll(("e" ^^ { _ => E } ||| "w" ^^ { _ => W } ||| "nw" ^^ { _ => NW } |||
        "ne" ^^ { _ => NE } ||| "sw" ^^ { _ => SW } ||| "se" ^^ { _ => SE }).+ ^^ Move, _).get).toList
  }

  override def calculate(): String = {
    val black = new MarkSet()
    val calcMoves = moves.map(_.calcMove)
    calcMoves.foreach(black.toggle)

    for (_ <- 1 to 100) {
      var changeSet = Set[(Int, Int)]()
      for (i <- (black.marked.map(_._1).min - 2) to (black.marked.map(_._1).max + 2);
           j <- (black.marked.map(_._2).min - 2) to (black.marked.map(_._2).max + 2)
           if ((i + j) % 2 + 2) % 2 == 0) {
        val count = black.count((i, j))
        if (!black.marked((i, j)) && count == 2)
          changeSet += i -> j
        else if (black.marked((i, j)) && (count == 0 || count > 2))
          changeSet += i -> j
      }
      changeSet.foreach(black.toggle)
      println(black.marked.size)
    }

    black.marked.size + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day24BS {
  def main(args: Array[String]): Unit = {
    new Day24BS().apply()
  }
}

