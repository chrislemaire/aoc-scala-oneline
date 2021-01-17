package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source
import scala.reflect.ClassTag

//noinspection SourceNotClosed
class Day20BS extends AdventOfCode {
  private val f = "misc/src/main/resources/day20.txt"

  sealed abstract class Dir
  object U extends Dir
  object D extends Dir
  object L extends Dir
  object R extends Dir

  case class Matrix2D[T](data: Array[Array[T]])(implicit _T: ClassTag[T]) {
    private def toMap: Map[(Int, Int), T] =
      data.zipWithIndex.flatMap(row =>
        row._1.zipWithIndex.map(col => (row._2, col._2) -> col._1))
        .toMap

    private def fromMap(m: Map[(Int, Int), T]): Matrix2D[T] =
      Matrix2D((0 to m.map(_._1._1).max).map(i =>
        (0 to m.map(_._1._2).max).map(j => m(i, j)).toArray).toArray)

    def n: Int = data.length

    def transpose: Matrix2D[T] = Matrix2D(data.transpose)
    def rotate: Matrix2D[T] =
      fromMap(toMap.map { case ((i, j), v) => (j, n - 1 - i) -> v })
    def rotate(n: Int): Matrix2D[T] =
      (0 to n).foldLeft(this)((m, _) => m.rotate)

    def cols(from: Int = 0, to: Int = n - 1): Matrix2D[T] =
      Matrix2D(data.map(_.drop(from).dropRight(n - 1 - to)))
    def rows(from: Int = 0, to: Int = n - 1): Matrix2D[T] =
      Matrix2D(data.drop(from).dropRight(n - 1 - to))

    def col(c: Int): Array[T] =
      cols(c, c).data.flatten
    def row(r: Int): Array[T] =
      rows(r, r).data.head

    def subarr(fromR: Int = 0, toR: Int = n - 1,
               fromC: Int = 0, toC: Int = n - 1): Matrix2D[T] =
      rows(fromR, toR).cols(fromC, toC)
  }

  case class TileOrientation(id: String, image: Matrix2D[Char]) {
    var neighbours: Map[Dir, Set[TileOrientation]] = Map()

    def l: String = image.col(0).mkString("")
    def r: String = image.col(image.n - 1).mkString("")
    def u: String = image.row(0).mkString("")
    def d: String = image.row(image.n - 1).mkString("")

    private def +=(t: (Dir, TileOrientation)): Unit =
      neighbours = neighbours.updated(t._1, neighbours.getOrElse(t._1, Set()) + t._2)

    def findNeighbours(others: List[TileOrientation]): Unit = {
      others.foreach(other => {
        if (u == other.d) this += U -> other
        if (d == other.u) this += D -> other
        if (l == other.r) this += L -> other
        if (r == other.l) this += R -> other
      })
    }

    def isTopLeftCorner: Boolean =
      neighbours.size == 2 && neighbours.contains(R) && neighbours.contains(D)

    def isTopRightCorner: Boolean =
      neighbours.size == 2 && neighbours.contains(L) && neighbours.contains(D)

    def isEdge: Boolean = neighbours.size == 3

    def isTopEdge: Boolean =
      isEdge && !neighbours.contains(U)

    def isCenter: Boolean =
      neighbours.size == 4

    def isTopRightCornerWithLeft(left: TileOrientation): Boolean =
      isTopRightCorner &&
        neighbours(L).contains(left)

    def isTopEdgeWithLeft(left: TileOrientation): Boolean =
      isTopEdge &&
        neighbours(L).contains(left)

    def isLeftEdgeWithUp(up: TileOrientation): Boolean =
      isEdge && !neighbours.contains(L) &&
        neighbours(U).contains(up)

    def isCenterWithUpAndLeft(up: TileOrientation, left: TileOrientation): Boolean =
      isCenter &&
        neighbours(U).contains(up) && neighbours(L).contains(left)

    def isRightEdgeWithUpAndLeft(up: TileOrientation, left: TileOrientation): Boolean =
      isEdge && !neighbours.contains(R) &&
        neighbours(U).contains(up) && neighbours(L).contains(left)

    def isLowerLeftCornerWithUp(up: TileOrientation): Boolean =
      neighbours.size == 2 && neighbours.contains(U) && neighbours.contains(R) &&
        neighbours(U).contains(up)

    def isLowerEdgeWithUpAndLeft(up: TileOrientation, left: TileOrientation): Boolean =
      neighbours.size == 3 && !neighbours.contains(D) &&
        neighbours(L).contains(left) && neighbours(U).contains(up)

    def isLowerRightCornerWithUpAndLeft(up: TileOrientation, left: TileOrientation): Boolean =
      neighbours.size == 2 && neighbours.contains(U) && neighbours.contains(L) &&
        neighbours(L).contains(left) && neighbours(U).contains(up)

    def actualImage: Matrix2D[Char] =
      image.subarr(1, image.n - 2, 1, image.n - 2)

    def ==(other: TileOrientation): Boolean =
      id == other.id
  }

  case class Tile(id: Int, image: Matrix2D[Char]) {
    val orientations: List[TileOrientation] =
      for (flip <- List(true, false);
           rot <- 0 to 3)
        yield TileOrientation(
          s"$id-$flip-$rot",
          (if (flip) image.transpose else image).rotate(rot))

    def ==(other: Tile): Boolean =
      id == other.id
  }

  private var nTiles: Int = 0
  private var tileSize: Int = 0
  private var tiles: List[Tile] = Nil

  private def assembleFirstRow(upperLeft: TileOrientation): Array[TileOrientation] = {
    val result = Array.ofDim[TileOrientation](nTiles)
    result.update(0, upperLeft)

    for (i <- 1 until nTiles - 1) {
      val left = result(i - 1)
      result.update(i, left.neighbours(R).filter(_.isTopEdgeWithLeft(left)).head)
    }

    val left = result(nTiles - 2)
    result.update(nTiles - 1, left.neighbours(R).filter(_.isTopRightCornerWithLeft(left)).head)

    result
  }

  private def assembleMiddleRow(previous: Array[TileOrientation]): Array[TileOrientation] = {
    val result = Array.ofDim[TileOrientation](nTiles)
    result.update(0, previous(0).neighbours(D).filter(_.isLeftEdgeWithUp(previous(0))).head)

    for (i <- 1 until nTiles - 1) {
      val left = result(i - 1)
      val up = previous(i)
      result.update(i, left.neighbours(R).filter(_.isCenterWithUpAndLeft(up, left)).head)
    }

    val left = result(nTiles - 2)
    val up = previous(nTiles - 1)
    result.update(nTiles - 1, left.neighbours(R).filter(_.isRightEdgeWithUpAndLeft(up, left)).head)

    result
  }

  private def assembleFinalRow(previous: Array[TileOrientation]): Array[TileOrientation] = {
    val result = Array.ofDim[TileOrientation](nTiles)
    result.update(0, previous(0).neighbours(D).filter(_.isLowerLeftCornerWithUp(previous(0))).head)

    for (i <- 1 until nTiles - 1) {
      val left = result(i - 1)
      val up = previous(i)
      result.update(i, left.neighbours(R).filter(_.isLowerEdgeWithUpAndLeft(up, left)).head)
    }

    val left = result(nTiles - 2)
    val up = previous(nTiles - 1)
    result.update(nTiles - 1, left.neighbours(R).filter(_.isLowerRightCornerWithUpAndLeft(up, left)).head)

    result
  }

  private def assembly: Tile = {
    tiles.foreach(tile => tile.orientations.foreach(_
      .findNeighbours(tiles.filterNot(_ == tile).flatMap(_.orientations))))
    val orientations = tiles.flatMap(_.orientations)

    // Build first row
    val result = Array.ofDim[TileOrientation](nTiles, nTiles)
    result.update(0, assembleFirstRow(orientations.filter(_.isTopLeftCorner).head))

    for (i <- 1 until nTiles - 1) {
      val up = result(i - 1)
      result.update(i, assembleMiddleRow(up))
    }

    result.update(nTiles - 1, assembleFinalRow(result(nTiles - 2)))

    val assembly = Array.ofDim[Char](nTiles * (tileSize - 2), nTiles * (tileSize - 2))
    for (tileI <- 0 until nTiles;
         tileJ <- 0 until nTiles;
         image = result(tileI)(tileJ).actualImage;
         i <- 0 until image.n;
         j <- 0 until image.n;
         c = image.data(i)(j)) {
      assembly(tileI * (tileSize - 2) + i).update(tileJ * (tileSize - 2) + j, c)
    }

    Tile(0, Matrix2D(assembly))
  }

  private def containsSeaMonster(arr: Array[Array[Char]], i: Int, j: Int, n: Int): Boolean =
    n - i >= 3 && n - j >= 20 &&
      arr(i + 1)(j + 0) == '#' &&
      arr(i + 2)(j + 1) == '#' &&
      arr(i + 2)(j + 4) == '#' &&
      arr(i + 1)(j + 5) == '#' &&
      arr(i + 1)(j + 6) == '#' &&
      arr(i + 2)(j + 7) == '#' &&
      arr(i + 2)(j + 10) == '#' &&
      arr(i + 1)(j + 11) == '#' &&
      arr(i + 1)(j + 12) == '#' &&
      arr(i + 2)(j + 13) == '#' &&
      arr(i + 2)(j + 16) == '#' &&
      arr(i + 1)(j + 17) == '#' &&
      arr(i + 0)(j + 18) == '#' &&
      arr(i + 1)(j + 18) == '#' &&
      arr(i + 1)(j + 19) == '#'

  override def read(sc: Scanner): Unit = {
    tiles = Source.fromFile(f).getLines().mkString("\n").split("\n\n")
      .filterNot(_.isEmpty)
      .map(tile => {
        val header = tile.split("\n").head
        val lines = tile.split("\n").tail

        val id = header.split("[ :]").tail.head.toInt

        Tile(id, Matrix2D(lines.map(_.toCharArray)))
      }).toList

    nTiles = Math.sqrt(tiles.size).toInt
    tileSize = tiles.head.image.n
  }

  override def calculate(): String = {
    val t = assembly
    val seaMonsters = t.orientations.map(o => {
      o -> (for (i <- 0 until o.image.n;
                 j <- 0 until o.image.n)
        yield containsSeaMonster(o.image.data, i, j, o.image.n)).count(identity)
    })

    println(seaMonsters)
    println(seaMonsters.maxBy(_._2)._1.image.data.map(_.mkString).mkString("\n"))

    t.image.data.map(_.count(_ == '#')).sum - seaMonsters.map(_._2).max * 15 + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day20BS {
  def main(args: Array[String]): Unit = {
    new Day20BS().apply()
  }
}
