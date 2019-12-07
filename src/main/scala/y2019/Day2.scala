package y2019

import scala.io.Source

class Day2(override val lines: List[String]) extends Solver() {
  override def solveA(): String = ((instr: List[Int]) =>
    LazyList.iterate[(List[Int], Int, Option[Int])]((instr, 0, None))({
      case (state, pos, None) => state.drop(pos) match {
        case 1 :: inp1 :: inp2 :: out :: _ => (state.updated(out, state(inp1) + state(inp2)), pos + 4, None)
        case 2 :: inp1 :: inp2 :: out :: _ => (state.updated(out, state(inp1) * state(inp2)), pos + 4, None)
        case 99 :: _ => (state, pos + 1, Some(state.head))
      }
    }).map(_._3).find(_.isDefined).get.get.toString)
    .apply(lines.head.split(",").map(_.toInt).toList
      .updated(1, 12).updated(2, 2))

  override def solveB(): String =
    (for (noun <- 0 to 99;
          verb <- 0 to 99) yield (noun, verb) -> ((instr: List[Int]) =>
      LazyList.iterate[(List[Int], Int, Option[Int])]((instr, 0, None))({
        case (state, pos, None) => state.drop(pos) match {
          case 1 :: inp1 :: inp2 :: out :: _ => (state.updated(out, state(inp1) + state(inp2)), pos + 4, None)
          case 2 :: inp1 :: inp2 :: out :: _ => (state.updated(out, state(inp1) * state(inp2)), pos + 4, None)
          case 99 :: _ => (state, pos + 1, Some(state.head))
          case _ => (state, pos, Some(0))
        }
      }).map(_._3).find(_.isDefined).get)
      .apply(lines.head.split(",").map(_.toInt).toList
        .updated(1, 12).updated(2, 2)))
      .find(_._2.contains(19690720)).map(t => t._1._1 * 100 + t._1._2).get.toString
}

object Day2 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/2019-2.txt").getLines().toList

    println(new Day2(lines).solveA())
    println(new Day2(lines).solveB())
  }
}

