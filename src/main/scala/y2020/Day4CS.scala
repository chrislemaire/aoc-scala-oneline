package y2020

import java.nio.file.Path
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed,DuplicatedCode
class Day4CS extends AdventOfCode with RegexParsers {
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
      })) (pp.keySet.intersect(Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")))) + ""

  override def file(): Path =
    Path.of(f)

  override def oneLine(): String =
    ((passports: List[String],
      yrP: (Int, Int) => Parser[Int],
      hgtP: Parser[Int]) => {
      passports.count(pp => parse((("byr:" ~> yrP(1920, 2002)) |||
        ("iyr:" ~> yrP(2010, 2020)) |||
        ("eyr:" ~> yrP(2020, 2030)) |||
        ("hgt:" ~> hgtP) |||
        ("hcl:#" ~> "[0-9a-f]{6}".r) |||
        ("ecl:" ~> ("amb" ||| "blu" ||| "brn" ||| "gry" ||| "grn" ||| "hzl" ||| "oth")) |||
        ("pid:" ~> "[0-9]{9}".r)).+, pp).map(_.size == 7).getOrElse(false)) + ""
    }) (Source.fromFile(f).getLines().mkString("\n").split("\n\n").filterNot(_.isEmpty).toList,
      (min, max) => ("\\d{4}".r ^^ {
        _.toInt
      }).filter(yr => min <= yr && yr <= max),
      ("\\d{3}".r <~ "cm" ^^ {
        _.toInt
      }).filter(hgt => 150 <= hgt && hgt <= 193) |||
        ("\\d{2}".r <~ "in" ^^ {
          _.toInt
        }).filter(hgt => 59 <= hgt && hgt <= 76))
}

object Day4CS {
  def main(args: Array[String]): Unit = {
    new Day4CS().applyBoth()
  }
}
