package y2019

import scala.io.Source

class Day7(override val lines: List[String]) extends Solver() {
  override def solveA(): String = ((instr: List[Int]) =>
    (0 to 4).permutations.map(settings =>
      settings.foldLeft(0)((amp, phase) =>
        LazyList.iterate[(List[Int], Int, List[Int], List[Int], Option[Int])]((instr, 0, Nil, List(phase, amp), None))({
          case (state, pos, output, inputs, None) => state.drop(pos) match {
            case 3 :: out :: _ => (state.updated(out, inputs.head), pos + 2, output, inputs.tail, None)

            case 4 :: out :: _ => (state, pos + 2, state(out) :: output, inputs, None)
            case 104 :: out :: _ => (state, pos + 2, out :: output, inputs, None)

            case 99 :: _ => (state, pos + 1, output, inputs, Some(state.head))

            case i :: inp1 :: inp2 :: out :: _ if List(1, 2, 7, 8).contains(i % 100) =>
              (state.updated(out, ((l: Int, r: Int) => i % 100 match {
                case 1 => l + r
                case 2 => l * r
                case 7 => if (l < r) 1 else 0
                case 8 => if (l == r) 1 else 0
              }).apply(
                if ((i / 100) % 10 == 0) state(inp1) else inp1,
                if ((i / 1000) % 10 == 0) state(inp2) else inp2
              )), pos + 4, output, inputs, None)

            case i :: c :: j :: _ if List(5, 6).contains(i % 100) =>
              (state, ((cond: Int, jump: Int) => i % 100 match {
                case 5 => if (cond != 0) jump else pos + 3
                case 6 => if (cond == 0) jump else pos + 3
              }).apply(
                if ((i / 100) % 10 == 0) state(c) else c,
                if ((i / 1000) % 10 == 0) state(j) else j
              ), output, inputs, None)
          }
        }).find(_._5.isDefined).flatMap(_._3.headOption).get
      )
    ).max.toString
    ).apply(lines.head.split(",").map(_.toInt).toList)

  override def solveB(): String = ((instr: List[Int]) =>
    (5 to 9).permutations.map(settings =>
      LazyList.iterate[(List[(List[Int], Int)], Int)](settings.map(instr.updated(8, _) -> 2).toList -> 0)({
        case ((state, pos) :: rest, amp) =>
          LazyList.iterate[(List[Int], Int, List[Int], List[Int], Option[Int])]((state, pos, Nil, List(amp), None))({
            case (state, pos, output, inputs, None) => state.drop(pos) match {
              case 3 :: out :: _ => (state.updated(out, inputs.head), pos + 2, output, inputs.tail, None)

              case 4 :: out :: _ => (state, pos + 2, state(out) :: output, inputs, None)
              case 104 :: out :: _ => (state, pos + 2, out :: output, inputs, None)

              case 99 :: _ => (state, pos + 1, output, inputs, Some(state.head))

              case i :: inp1 :: inp2 :: out :: _ if List(1, 2, 7, 8).contains(i % 100) =>
                (state.updated(out, ((l: Int, r: Int) => i % 100 match {
                  case 1 => l + r
                  case 2 => l * r
                  case 7 => if (l < r) 1 else 0
                  case 8 => if (l == r) 1 else 0
                }).apply(
                  if ((i / 100) % 10 == 0) state(inp1) else inp1,
                  if ((i / 1000) % 10 == 0) state(inp2) else inp2
                )), pos + 4, output, inputs, None)

              case i :: c :: j :: _ if List(5, 6).contains(i % 100) =>
                (state, ((cond: Int, jump: Int) => i % 100 match {
                  case 5 => if (cond != 0) jump else pos + 3
                  case 6 => if (cond == 0) jump else pos + 3
                }).apply(
                  if ((i / 100) % 10 == 0) state(c) else c,
                  if ((i / 1000) % 10 == 0) state(j) else j
                ), output, inputs, None)
            }
          }).find(t => t._5.isDefined || t._3.nonEmpty)
            .map(t =>
              t._5.fold(
                (rest ++ List((t._1, t._2)), t._3.head)
              )(_ =>
                (rest, amp)
              )
            ).get
      }).find(_._1.isEmpty).map(_._2).get
    ).max.toString
    ).apply(lines.head.split(",").map(_.toInt).toList)
}


object Day7 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/resources/2019-7.txt").getLines().toList

    println(new Day7(lines).solveA())
    println(new Day7(lines).solveB())
  }
}



