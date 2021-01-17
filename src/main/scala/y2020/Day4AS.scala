package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day4AS extends AdventOfCode {
  private final val f = "misc/src/main/resources/day4.txt"

  private var passports: List[Map[String, String]] = Nil

  override def read(sc: Scanner): Unit =
    passports = Source.fromFile(f).getLines().mkString("\n")
      .split("\n\n")
      .filterNot(_.isEmpty)
      .map(_
        .replaceAll("\n", " ")
        .split("\\s+")
        .map(rule => (rule.split(":").head, rule.split(":").tail.head))
        .toMap)
      .toList

  override def calculate(): String =
    passports.count(pp =>
      pp.keySet.intersect(Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")).size == 7) + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: List[Map[String, String]]) => {
        /* Process input */
        input.count(pp =>
          pp.keySet.intersect(Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")).size == 7) + ""
      }) (
        /* Read input */
        lines
          .filterNot(_.isEmpty)
          .map(_
            .replaceAll("\n", " ")
            .split("\\s+")
            .map(rule => (rule.split(":").head, rule.split(":").tail.head))
            .toMap)
          .toList
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().mkString("\n")
        .split("\n\n").toList
    ) + ""
}

object Day4AS {
  def main(args: Array[String]): Unit = {
    new Day4AS().applyBoth()
  }
}
