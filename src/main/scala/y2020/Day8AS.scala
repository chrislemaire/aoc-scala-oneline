package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day8AS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day8.txt"

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((instr: Map[Int, (String, Int)]) =>
        /* Process input, (seen, acc, instruction) */
        Iterator.iterate((Set[Int](), 0, 0))(t => (t, instr.get(t._3)) match {
          case ((seen, acc, idx), Some(("jmp", n))) => (seen ++ Set(idx), acc, idx + n)
          case ((seen, acc, idx), Some(("nop", _))) => (seen ++ Set(idx), acc, idx + 1)
          case ((seen, acc, idx), Some(("acc", n))) => (seen ++ Set(idx), acc + n, idx + 1)
        }).find(t => t._1(t._3))
          .map(_._2)
          .getOrElse(-1) + ""
        ) (
        /* Read input */
        lines
          .map(parse(("acc" ||| "nop" ||| "jmp") ~ ("[-+]\\d+".r ^^ {
            _.stripPrefix("+").toInt
          }), _).get)
          .map({ case op ~ n => (op, n) })
          .zipWithIndex.map(_.swap).toMap
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day8AS {
  def main(args: Array[String]): Unit = {
    println(new Day8AS().oneLine())
  }
}
