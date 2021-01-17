package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source

abstract class P extends (String => Stream[String]) {
  def ~(p: => P): P = s1 => apply(s1).flatMap(s2 => p.apply(s2))
  def |(p: => P): P = s1 => apply(s1) #::: p.apply(s1)
  def ? : P = s1 => s1 #:: apply(s1)

  def consumesAll(s: String): Boolean =
    apply(s).contains("")
}

object P {
  def apply(s: String): P = s1 =>
    if (s1.startsWith(s)) s1.substring(s.length) #:: Stream.empty
    else Stream.empty

  def apply(c: Char): P = s1 =>
    if (s1.length > 0 && s1.charAt(0) == c) s1.substring(1) #:: Stream.empty
    else Stream.empty
}

class Day19BS extends AdventOfCode {
  private val f = "misc/src/main/resources/day19.txt"

  override def read(sc: Scanner): Unit = ???
  override def calculate(): String = ???
  override def file(): Path = Paths.get(f)

  private def andP(rules: Map[Int, List[String]], p11: Int, p12: Int): P =
    createP(p11, rules) ~ createP(p12, rules)

  private def recP(l: => P, r: => P): P =
    P("") | (l ~ recP(l, r) ~ r)

  private def createP(i: Int, rules: Map[Int, List[String]]): P = {
    if (i == 8)
      createP(42, rules) ~ createP(8, rules).?
    else if (i == 11)
      createP(42, rules) ~ recP(createP(42, rules), createP(31, rules)) ~ createP(31, rules)
    else
      rules.get(i).map({
      case s :: Nil if s.matches("\\d+") => createP(s.toInt, rules)
      case s :: Nil => P(s.charAt(1))
      case p1 :: p2 :: Nil => andP(rules, p1.toInt, p2.toInt)
      case p1 :: "|" :: p2 :: Nil => createP(p1.toInt, rules) | createP(p2.toInt, rules)
      case p11 :: p12 :: "|" :: p21 :: p22 :: Nil => andP(rules, p11.toInt, p12.toInt) | andP(rules, p21.toInt, p22.toInt)
    }).get
  }

  //noinspection SourceNotClosed
  override def oneLine(): String =
    ((lines: List[List[String]]) => {
      ((p: P, input: List[String]) =>
        /* Process input */
        input.count(s => p.consumesAll(s))
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

object Day19BS {
  def main(args: Array[String]): Unit = {
    println(new Day19BS().oneLine())
  }
}



