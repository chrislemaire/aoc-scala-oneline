package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day2AS extends AdventOfCode {
  private var l: List[Instance] = Nil

  private case class Instance(min: Int, max: Int, c: Char, s: String) {
    def isValid: Boolean =
      min <= s.count(_ == c) && s.count(_ == c) <= max
  }

  private final val f = "misc/src/main/resources/day2.txt"

  override def read(sc: Scanner): Unit =
    while (sc.hasNextLine) {
      val line = sc.nextLine()
      val splits = line.replaceAll("[:-]", " ").split("\\s+")
      l = Instance(splits(0).toInt, splits(1).toInt, splits(2)(0), splits(3)) :: l
    }

  override def calculate(): String =
    l.count(_.isValid) + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: List[(Int, Int, Char, String)]) => {
        /* Process input */
        input.count({
          case (min, max, c, s) =>
            min <= s.count(_ == c) && s.count(_ == c) <= max
        })
      })(
        /* Read input */
        lines.map(line => line.split("\\s+").toList)
          .map({
            case min :: max :: c :: s :: Nil =>
              (min.toInt, max.toInt, c(0), s)
          })
      )
    })(
      /* Read file */
      Source.fromFile(f).getLines().toList
        .filterNot(_.isEmpty)
        .map(_.replaceAll("[:-]", " "))
    ) + ""
}

object Day2AS {
  def main(args: Array[String]): Unit = {
    new Day2AS().applyBoth()
  }
}
