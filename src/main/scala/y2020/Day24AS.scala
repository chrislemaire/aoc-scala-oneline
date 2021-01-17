package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day24AS extends AdventOfCode with RegexParsers {
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
  }

  private var moves: List[Move] = Nil

  override def read(sc: Scanner): Unit = {
    moves = Source.fromFile(f).getLines()
      .map(parseAll(("e" ^^ { _ => E } ||| "w" ^^ { _ => W } ||| "nw" ^^ { _ => NW } |||
        "ne" ^^ { _ => NE } ||| "sw" ^^ { _ => SW } ||| "se" ^^ { _ => SE }).+ ^^ Move, _).get).toList
  }

  override def calculate(): String = {
    val black = new MarkSet()
    for (move <- moves) {
      black.toggle(move.calcMove)
    }
    black.marked.size + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day24AS {
  def main(args: Array[String]): Unit = {
    new Day24AS().apply()
  }
}
