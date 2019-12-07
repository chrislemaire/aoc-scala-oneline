package y2019

import scala.io.Source

class Day6(override val lines: List[String]) extends Solver() {
  override def solveA(): String =
    ((orbits: Map[String, String]) => orbits
      .keySet.toList
      .map(e => LazyList.iterate(e)(res =>
        orbits.getOrElse(res, "")
      ).takeWhile(_.nonEmpty).size - 1)
      .sum.toString)
      .apply(lines.map(_.split(')')).map(s => s(1) -> s(0)).toMap)

  override def solveB(): String = ((g: Map[String, Set[String]]) =>
    LazyList.iterate[(Set[String], Int)]((Set("JN3"), 1))(prev =>
      (prev._1.flatMap(p => g.getOrElse(p, Set())), prev._2 + 1))
      .find(!_._1.contains("T1T")).get._2.toString
    ).apply(lines.map(_.split(')'))
    .flatMap(s => List(s(0) -> s(1), s(1) -> s(0)))
    .groupBy(_._1)
    .map((t: (String, List[(String, String)])) => t.copy(_2 = t._2.map(_._2).toSet)))
}

object Day6 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/2019-6.txt").getLines().toList

    println(new Day6(lines).solveA())
    println(new Day6(lines).solveB())
  }
}

