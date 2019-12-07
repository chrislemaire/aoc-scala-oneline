package y2019

import scala.io.Source

class Day5(override val lines: List[String]) extends Solver() {
  override def solveA(): String = ((instr: List[Int]) =>
    LazyList.iterate[(List[Int], Int, List[Int], Option[Int])]((instr, 0, Nil, None))({
      case (state, pos, output, None) => state.drop(pos) match {
        case 3 :: out :: _ => (state.updated(out, 1), pos + 2, output, None)

        case 4 :: out :: _ => (state, pos + 2, state(out) :: output, None)
        case 104 :: out :: _ => (state, pos + 2, out :: output, None)

        case 99 :: _ => (state, pos + 1, output, Some(state.head))

        case i :: inp1 :: inp2 :: out :: _ => (state.updated(out, ((l: Int, r: Int) => i % 100 match {
          case 1 => l + r
          case 2 => l * r
        }).apply(
          if ((i / 100) % 10 == 0) state(inp1) else inp1,
          if ((i / 1000) % 10 == 0) state(inp2) else inp2
        )), pos + 4, output, None)
      }
    }).takeWhile(_._4.isEmpty).map(_._3).last.mkString("\n"))
    .apply(lines.head.split(",").map(_.toInt).toList)

  override def solveB(): String = ((instr: List[Int]) =>
    LazyList.iterate[(List[Int], Int, List[Int], Option[Int])]((instr, 0, Nil, None))({
      case (state, pos, output, None) => state.drop(pos) match {
        case 3 :: out :: _ => (state.updated(out, 5), pos + 2, output, None)

        case 4 :: out :: _ => (state, pos + 2, state(out) :: output, None)
        case 104 :: out :: _ => (state, pos + 2, out :: output, None)

        case 99 :: _ => (state, pos + 1, output, Some(state.head))

        case i :: inp1 :: inp2 :: out :: _ if List(1, 2, 7, 8).contains(i % 100) =>
          (state.updated(out, ((l: Int, r: Int) => i % 100 match {
            case 1 => l + r
            case 2 => l * r
            case 7 => if (l < r) 1 else 0
            case 8 => if (l == r) 1 else 0
          }).apply(
            if ((i / 100) % 10 == 0) state(inp1) else inp1,
            if ((i / 1000) % 10 == 0) state(inp2) else inp2
          )), pos + 4, output, None)

        case i :: c :: j :: _ if List(5, 6).contains(i % 100) =>
          (state, ((cond: Int, jump: Int) => i % 100 match {
            case 5 => if (cond != 0) jump else pos + 3
            case 6 => if (cond == 0) jump else pos + 3
          }).apply(
            if ((i / 100) % 10 == 0) state(c) else c,
            if ((i / 1000) % 10 == 0) state(j) else j
          ), output, None)
      }
    }).takeWhile(_._4.isEmpty).map(_._3).last.mkString("\n"))
    .apply(lines.head.split(",").map(_.toInt).toList)
}

object Day5 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/2019-5.txt").getLines().toList

    println(new Day5(lines).solveA())
    println()
    println(new Day5(lines).solveB())
  }
}

