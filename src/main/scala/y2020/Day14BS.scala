package y2020

import java.lang.Long.parseLong
import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day14BS extends AdventOfCode with RegexParsers {
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
        data.foldLeft((Map[Long, Long](), (n => List(n)): Long => List[Long]))((acc, curr) => curr match {
          case Left(mask) => acc.copy(_2 = mask.reverse.zipWithIndex
            .groupBy(_._1).mapValues(_.map(_._2).toList)
            .foldLeft((n => List(n)): Long => List[Long])((f, curr) => curr match {
              case ('X', l) => n => f(n)
                .flatMap(n => (0L until l.map(_ => 2L).product)
                  .map(i => l.zip(Iterator.iterate(1L)(_ << 1).takeWhile(_ <= i).toList).foldLeft(n)((n, d) =>
                    if ((d._2 & i) > 0) n ^ Iterator.fill(d._1)(2L).product else n)))
              case ('1', l) => n => f(n).map(_ | parseLong((0 until 36).map(i => if (l.toSet(35 - i)) '1' else '0').mkString(""), 2))
              case ('0', _) => f
            }))
          case Right((addr, v)) => acc.copy(_1 = acc._2(addr).foldLeft(acc._1)((a, addr) => a.updated(addr, v)))
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

object Day14BS {
  def main(args: Array[String]): Unit = {
    println(new Day14BS().oneLine())
  }
}
