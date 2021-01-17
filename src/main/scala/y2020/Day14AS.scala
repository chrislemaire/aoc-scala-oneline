package y2020

import java.lang.Long.parseLong
import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day14AS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day14.txt"

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((data: List[Either[String, (Long, Long)]]) =>
        /* Process input */
        data.foldLeft((Map[Long, Long](), identity[Long]: Long => Long))((acc, curr) => curr match {
          case Left(mask) => acc.copy(_2 = mask.reverse.zipWithIndex
            .filter(_._1 != 'X')
            .groupBy(_._1).mapValues(_.map(_._2).toSet)
            .foldLeft(identity[Long]: Long => Long)((f, curr) => curr match {
              case ('1', l) => n => f(n) | parseLong((0 until 36).map(i => if (l(35 - i)) '1' else '0').mkString(""), 2)
              case ('0', l) => n => f(n) & parseLong((0 until 36).map(i => if (l(35 - i)) '0' else '1').mkString(""), 2)
            }))
          case Right((addr, v)) => acc.copy(_1 = acc._1.updated(addr, acc._2(v)))
        })._1.values.sum
        ) (
        /* Read input */
        lines.map(line => parse(("mask" ~ "=" ~> "\\w+".r.^^(Left[String, (Long, Long)]))
          ||| ((("mem[" ~> "\\d+".r.^^(_.toInt) <~ "]" ~ "=")
          ~ "\\d+".r.^^(_.toInt))).^^(t => Right[String, (Long, Long)]((t._1, t._2))), line).get)
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
        .filterNot(_.isEmpty)
    ) + ""
}

object Day14AS {
  def main(args: Array[String]): Unit = {
    println(new Day14AS().oneLine())
  }
}
