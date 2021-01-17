package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day4BS extends AdventOfCode with RegexParsers {
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
      ((keys: Set[String]) => keys.size == 7 && pp.filterKeys(keys).forall({
        case ("byr", s) => s.matches("\\d{4}") && 1920 <= s.toInt && s.toInt <= 2002
        case ("iyr", s) => s.matches("\\d{4}") && 2010 <= s.toInt && s.toInt <= 2020
        case ("eyr", s) => s.matches("\\d{4}") && 2020 <= s.toInt && s.toInt <= 2030
        case ("hgt", s) =>
          (s.matches("\\d{3}cm") && 150 <= s.substring(0, 3).toInt && s.substring(0, 3).toInt <= 193) ||
            (s.matches("\\d{2}in") && 59 <= s.substring(0, 2).toInt && s.substring(0, 2).toInt <= 76)
        case ("hcl", s) => s.matches("#[0-9a-f]{6}")
        case ("ecl", s) => s.matches("(amb|blu|brn|gry|grn|hzl|oth)")
        case ("pid", s) => s.matches("[0-9]{9}")
      }))(pp.keySet.intersect(Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")))) + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((lines: List[String]) => {
      ((input: List[Map[String, String]]) => {
        /* Process input */
        input.count(pp =>
          ((keys: Set[String]) => keys.size == 7 && pp.filterKeys(keys).forall({
            case ("byr", s) => s.matches("\\d{4}") && 1920 <= s.toInt && s.toInt <= 2002
            case ("iyr", s) => s.matches("\\d{4}") && 2010 <= s.toInt && s.toInt <= 2020
            case ("eyr", s) => s.matches("\\d{4}") && 2020 <= s.toInt && s.toInt <= 2030
            case ("hgt", s) =>
              (s.matches("\\d{3}cm") && 150 <= s.substring(0, 3).toInt && s.substring(0, 3).toInt <= 193) ||
                (s.matches("\\d{2}in") && 59 <= s.substring(0, 2).toInt && s.substring(0, 2).toInt <= 76)
            case ("hcl", s) => s.matches("#[0-9a-f]{6}")
            case ("ecl", s) => s.matches("(amb|blu|brn|gry|grn|hzl|oth)")
            case ("pid", s) => s.matches("[0-9]{9}")
          }))(pp.keySet.intersect(Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")))) + ""
      }) (
        /* Read input */
        lines
          .filterNot(_.isEmpty)
          .map(_
            .replaceAll("\n", " ")
            .split("\\s+")
            .map(rule => (rule.split(":").head, rule.split(":").tail.head))
            .toMap)
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().mkString("\n")
        .split("\n\n").toList
    ) + ""
}

object Day4BS {
  def main(args: Array[String]): Unit = {
    new Day4BS().applyBoth()
  }
}
