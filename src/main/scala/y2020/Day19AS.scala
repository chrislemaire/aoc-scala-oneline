package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source
import scala.util.matching.Regex

class Day19AS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day19.txt"

  override protected val whiteSpace: Regex = "\\s*".r

  private var rul: List[String] = Nil
  private var inp: List[String] = Nil

  override def read(sc: Scanner): Unit = ???
  override def calculate(): String = ???
  override def file(): Path = Paths.get(f)

  private def andP(rules: Map[Int, List[String]], p11: Int, p12: Int): Parser[String] =
    createP(p11, rules) ~ createP(p12, rules) ^^ { case a ~ b => a + b }

  private def createP(i: Int, rules: Map[Int, List[String]]): Parser[String] =
    rules.get(i).map({
      case s :: Nil if s.matches("\\d+") => createP(s.toInt, rules)
      case s :: Nil => s"[${s.charAt(1)}]".r ^^ identity
      case p1 :: p2 :: Nil => andP(rules, p1.toInt, p2.toInt)
      case p1 :: "|" :: p2 :: Nil => createP(p1.toInt, rules) ||| createP(p2.toInt, rules)
      case p11 :: p12 :: "|" :: p21 :: p22 :: Nil => andP(rules, p11.toInt, p12.toInt) ||| andP(rules, p21.toInt, p22.toInt)
    }).get

  private def createRegex(rules: Map[Int, String]): String = {
    val rulesTransformed = rules.mapValues(_
      .replaceAll("(\\d+)", "`\1")
      .replaceAll("(.*)[|](.*)", "(\1)|(\2)")
      .replaceAll("\\s+", ""))

    rules.keys.foldLeft(rules(0))((rule, _) =>
      rules.keys.foldLeft(rules(0))((rule, i) =>
        rule.replaceAll(s"$i", rulesTransformed(i))))
  }

  //noinspection SourceNotClosed
  override def oneLine(): String =
    ((lines: List[List[String]]) => {
      ((p: Parser[String], input: List[String]) =>
        /* Process input */
        input.count(parse("^".r ~> p <~ "$".r, _).successful)
        ) (
        /* Read input */
        createP(0, lines.head.map(line => (line.split("[:]").head.toInt,
          line.split("[:]").last.trim.split("\\s+").toList)).toMap),
        lines.drop(1).head
      )
    }) (
      /* Read file */
      Source.fromFile(f).getLines().mkString("\n").split("\n\n").toList
        .filterNot(_.isEmpty).map(_.split("\n").toList)
    ) + ""
}

object Day19AS {
  def main(args: Array[String]): Unit = {
    println(new Day19AS().oneLine())
  }
}

