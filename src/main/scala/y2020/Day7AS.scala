package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day7AS extends AdventOfCode with RegexParsers {
  private final val f = "misc/src/main/resources/day7.txt"

  private var answers: List[List[Set[Char]]] = Nil

  override def read(sc: Scanner): Unit =
    ???

  override def calculate(): String =
    ???

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) =>
      ((input: List[(String, Set[String])]) =>
        /* Process input */
        Iterator
          .iterate((Set[String](), Set("shiny gold")))(
            bags => bags._2 -> (input.filter(_._2.intersect(bags._2).nonEmpty).map(_._1).toSet ++ bags._2))
          .find(bags => bags._1 == bags._2)
          .map(bags => bags._1.size - 1)
          .getOrElse(0) + ""
      ) (
        /* Read input */
        lines
          .map(line => parse(
            ("\\w+\\s\\w+".r <~ "bags contain") ~
              (rep1sep("\\d+".r.^^(_.toInt) ~ ("\\w+\\s\\w+".r <~ "bag[s]?".r), "[,.]".r) |||
              ("no other bags." ^^ (_ => List[Int ~ String]()))), line))
          .flatMap({
            case Success(id ~ contains, _) => Some(id -> contains.map(_._2).toSet)
            case _ => None
          })
      )
    ) (
      /* Read file */
      Source.fromFile(f).getLines().toList
    ) + ""
}

object Day7AS {
  def main(args: Array[String]): Unit = {
    println(new Day7AS().oneLine())
  }
}
