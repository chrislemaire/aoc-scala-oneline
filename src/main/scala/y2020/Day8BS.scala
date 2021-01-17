package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day8BS extends AdventOfCode with RegexParsers {
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
        instr.filter(e => Set("jmp", "nop")(e._2._1))
          .map(e => instr.updated(e._1, (if (e._2._1 == "jmp") "nop" else "jmp", e._2._2)))
          .flatMap(instrAdjusted =>
            Iterator.iterate((Set[Int](), 0, 0))(t => (t, instrAdjusted.get(t._3)) match {
              case ((seen, acc, idx), Some(("jmp", n))) => (seen ++ Set(idx), acc, idx + n)
              case ((seen, acc, idx), Some(("nop", _))) => (seen ++ Set(idx), acc, idx + 1)
              case ((seen, acc, idx), Some(("acc", n))) => (seen ++ Set(idx), acc + n, idx + 1)
            }).find(t => t._1(t._3) || t._3 == instr.keys.max + 1))
          .find(_._3 == instr.keys.max + 1)
          .head._2 + ""
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

object Day8BS {
  def main(args: Array[String]): Unit = {
    println(new Day8BS().oneLine())
  }
}

