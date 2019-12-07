package y2019

import scala.io.Source

class Day4(override val lines: List[String]) extends Solver {
  override def solveA(): String =
    lines.map(_.split('-').map(_.toInt).toList).map({
      case min :: max :: _ => (min to max).map(_.toString)
        .count(s => s.toSeq.sliding(2, 1).forall(i => i(0) <= i(1))
          && s.toSeq.groupBy(c => c).values.map(_.size).exists(_ >= 2))
    }).head.toString

  override def solveB(): String =
    lines.map(_.split('-').map(_.toInt).toList).map({
      case min :: max :: _ => (min to max).map(_.toString)
        .count(s => s.toSeq.sliding(2, 1).forall(i => i(0) <= i(1))
          && s.toSeq.groupBy(c => c).values.map(_.size).exists(_ == 2))
    }).head.toString
}

object Day4 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/2019-4.txt").getLines().toList

    println(new Day4(lines).solveA())
    println(new Day4(lines).solveB())
  }
}
