package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day20AS extends AdventOfCode {
  private val f = "misc/src/main/resources/day20.txt"

  case class Piece(n: Int, id: Long, a: Array[Array[Char]]) {
    def edges: Map[Orientation, Long] =
      (for (f <- Set(true, false);
            rot <- Set(0, 1, 2, 3)) yield Orientation(n, rot, f))
        .map(o => o -> o.firstLine(this)).toMap
        .mapValues(a => a.foldLeft((0L, 1L))((acc, a) => (acc._1 + (if (a == '#') acc._2 else 0), acc._2 * 2))._1)
  }

  case class Orientation(n: Int, rots: Int, flip: Boolean) {
    def rotateMatrix[A](mat: Map[(Int, Int), Char]): Map[(Int, Int), Char] =
      mat.map { case ((i, j), v) => (j, n - 1 - i) -> v }

    def iterate(p: Piece): Array[Array[Char]] = {
      val matrix = p.a.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(col => (row._2, col._2) -> col._1)).toMap

      val rotated = (0 until rots).foldLeft(matrix)((m, _) => rotateMatrix(m))
      val rotatedArr = (0 until n).map(i => (0 until n).map(j => rotated(i, j)).toArray).toArray
      if (flip) rotatedArr.transpose else rotatedArr
    }

    def firstLine(p: Piece): Array[Char] =
      iterate(p).head
  }

  private var tiles: List[Piece] = Nil

  override def read(sc: Scanner): Unit = {
    tiles = Source.fromFile(f).getLines().mkString("\n").split("\n\n")
      .filterNot(_.isEmpty)
      .map(tile => {
        val header = tile.split("\n").head
        val lines = tile.split("\n").tail

        val id = header.split("[ :]").tail.head.toInt
        val n = lines.head.length

        Piece(n, id, lines.map(_.toCharArray))
      }).toList
  }

  override def calculate(): String = {
    val combinations = for (tile1 <- tiles;
         tile2 <- tiles
         if tile1.id != tile2.id &&
           tile1.edges.values.toSet.intersect(tile2.edges.values.toSet).nonEmpty)
      yield (tile1, tile2)

    combinations.groupBy(_._1).mapValues(_.size).filter(_._2 == 2)
      .map(_._1.id).product + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day20AS {
  def main(args: Array[String]): Unit = {
    new Day20AS().apply()
  }
}
