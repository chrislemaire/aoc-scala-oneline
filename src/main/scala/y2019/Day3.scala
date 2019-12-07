package y2019

import scala.io.Source

class Day3(override val lines: List[String]) extends Solver {

  override def solveA(): String =
    ((positions: List[Map[(Int, Int), Int]]) =>
      positions.map(_.keys.toSet).reduce((l, r) => l.intersect(r))
        .map(t => Math.abs(t._1) + Math.abs(t._2))
        .min.toString).apply(
      ((dx: Int, dy: Int, d: Int, from: (Int, Int), steps: Int) =>
        (1 to steps).map(i => ((from._1 + dx * i, from._2 + dy * i), d + i)).toMap) match {
        case createMapping =>
          lines.map(_.split(",").map(move => (move.head, move.tail.toInt)).toList)
            .map(moves => moves.foldLeft[((Int, Int), Int, Map[(Int, Int), Int])](((0, 0), 0, Map()))((l, r) => (l, r) match {
              case ((p@(x, y), d, m), ('U', steps)) => ((x, y + steps), d + steps, m ++ createMapping(0, 1, d, p, steps))
              case ((p@(x, y), d, m), ('D', steps)) => ((x, y - steps), d + steps, m ++ createMapping(0, -1, d, p, steps))
              case ((p@(x, y), d, m), ('L', steps)) => ((x - steps, y), d + steps, m ++ createMapping(-1, 0, d, p, steps))
              case ((p@(x, y), d, m), ('R', steps)) => ((x + steps, y), d + steps, m ++ createMapping(1, 0, d, p, steps))
            })._3)
      })

  override def solveB(): String =
    ((positions: List[Map[(Int, Int), Int]]) =>
      positions.map(_.keys.toSet).reduce((l, r) => l.intersect(r))
        .map(t => positions.map(_.find(kv => kv._1 == t).map(_._2).getOrElse(100000000)).sum)
        .min.toString).apply(
      ((dx: Int, dy: Int, d: Int, from: (Int, Int), steps: Int) =>
        (1 to steps).map(i => ((from._1 + dx * i, from._2 + dy * i), d + i)).toMap) match {
        case positions =>
          lines.map(_.split(",").map(move => (move.head, move.tail.toInt)).toList)
            .map(moves => moves.foldLeft[((Int, Int), Int, Map[(Int, Int), Int])](((0, 0), 0, Map()))((l, r) => (l, r) match {
              case ((p@(x, y), d, m), ('U', steps)) => ((x, y + steps), d + steps, m ++ positions(0, 1, d, p, steps))
              case ((p@(x, y), d, m), ('D', steps)) => ((x, y - steps), d + steps, m ++ positions(0, -1, d, p, steps))
              case ((p@(x, y), d, m), ('L', steps)) => ((x - steps, y), d + steps, m ++ positions(-1, 0, d, p, steps))
              case ((p@(x, y), d, m), ('R', steps)) => ((x + steps, y), d + steps, m ++ positions(1, 0, d, p, steps))
            })._3)
      })
}

object Day3 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/2019-3.txt").getLines().toList

    println(new Day3(lines).solveA())
    println(new Day3(lines).solveB())
  }
}
