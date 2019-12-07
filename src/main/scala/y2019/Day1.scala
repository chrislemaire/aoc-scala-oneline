package y2019

import scala.io.Source

class Day1(override val lines: List[String]) extends Solver() {
  private val weights: List[Int] = lines.map(_.trim.toInt)

  override def solveA(): String =
    weights.map(Math.floorDiv(_, 3) - 2).sum.toString

  override def solveB(): String =
    weights.flatMap(w => LazyList.iterate(Math.floorDiv(w, 3) - 2)(f => Math.floorDiv(f, 3) - 2).takeWhile(_ > 0)).sum.toString
}

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/2019-1.txt").getLines().toList

    println(new Day1(lines).solveA())
    println(new Day1(lines).solveB())
  }
}
