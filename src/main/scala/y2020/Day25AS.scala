package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day25AS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day25.txt"

  private var pk1: Long = 0
  private var pk2: Long = 0

  override def read(sc: Scanner): Unit = {
    val lines = Source.fromFile(f).getLines().toArray
    pk1 = lines(0).toInt
    pk2 = lines(1).toInt
  }

  def getLoopSize(subj: Long, pk: Long): Long =
    Iterator.iterate(1L)(value => (value * subj) % 20201227L).takeWhile(_ != pk).size

  override def calculate(): String = {
    val l1 = getLoopSize(7L, pk1)
    val l2 = getLoopSize(7L, pk2)

    (1L to l2).foldLeft(1L)((value, _) => ((value * pk1) + 20201227L) % 20201227L) + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day25AS {
  def main(args: Array[String]): Unit = {
    new Day25AS().apply()
  }
}

