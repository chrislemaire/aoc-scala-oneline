package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.collection.mutable
import scala.io.Source

//noinspection SourceNotClosed
class Day22AS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day22.txt"

  case class Player(deck: mutable.Queue[Int]) {
    def calc(mine: Int, other: Int): Unit = {
      if (other < mine) deck += mine += other
    }

    def next: Int = deck.dequeue()
    def hasNext: Boolean = deck.nonEmpty

    def score: Long =
      deck.reverse.zipWithIndex.map(t => t._1.toLong * (t._2 + 1)).sum
  }

  private var p1: Player = null
  private var p2: Player = null

  override def read(sc: Scanner): Unit = {
    val players = Source.fromFile(f).getLines().mkString("\n").split("\n\n")
      .map(_.split("\n").tail)
      .map(l => {
        val p = Player(mutable.Queue())
        for (i <- l.map(_.toInt)) p.deck.enqueue(i)
        p
      })

    p1 = players(0)
    p2 = players(1)
  }

  override def calculate(): String = {
    while (p1.hasNext && p2.hasNext) {
      val p1g = p1.next
      val p2g = p2.next

      p1.calc(p1g, p2g)
      p2.calc(p2g, p1g)
    }

    if (p1.hasNext) p1.score + ""
    else p2.score + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day22AS {
  def main(args: Array[String]): Unit = {
    new Day22AS().apply()
  }
}
