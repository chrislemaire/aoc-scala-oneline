package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day13AS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day13.txt"

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((leaving: Long, buses: List[Option[Long]]) =>
        /* Process input */
        Some(buses.flatten.map(id => (id, Math.ceil(leaving.toDouble / id).toLong * id)).minBy(_._2))
          .map(t => t._1 * (t._2 - leaving)).get
        ) (
        /* Read input */
        lines.head.toLong,
          lines.drop(1).head.split(",").map(l => if (l == "x") None else Some(l.toLong)).toList
      )
      ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day13AS {
  def main(args: Array[String]): Unit = {
    println(new Day13AS().oneLine())
  }
}
