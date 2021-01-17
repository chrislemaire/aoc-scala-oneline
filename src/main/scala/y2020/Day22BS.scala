package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.collection.mutable
import scala.io.Source

//noinspection SourceNotClosed
class Day22BS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day22.txt"

  class Result(var winner: Int = 0)

  case class Player(deck: mutable.Queue[Int]) {
    def calc(mine: Int, other: Int): Unit =
      if (other < mine) take(mine, other)

    def take(big: Int, small: Int): Unit =
      deck += big += small

    def next: Int = deck.dequeue()
    def hasNext: Boolean = deck.nonEmpty

    def score: Long =
      deck.reverse.zipWithIndex.map(t => t._1.toLong * (t._2 + 1)).sum

    def toState: String =
      deck.mkString("-")

    def copy(n: Int): Player = {
      val p = Player(mutable.Queue())
      for (i <- 0 until n) p.deck.enqueue(deck.drop(i).head)
      p
    }
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

  private def mkGameString(p1: Player, p2: Player): String =
    p1.toState + "." + p2.toState

  private def playRecursiveGame(p1: Player, p2: Player): Int = {
    val result = new Result()
    val previousGames: mutable.Set[String] = mutable.Set()

    while (p1.hasNext && p2.hasNext) {
      if (previousGames(mkGameString(p1, p2)))
        return 1

      previousGames += mkGameString(p1, p2)

      val p1g = p1.next
      val p2g = p2.next

      var winner: Int = 1
      if (p1g <= p1.deck.size && p2g <= p2.deck.size) {
        winner = playRecursiveGame(p1.copy(p1g), p2.copy(p2g))
      } else if (p2g > p1g) {
        winner = 2
      }

      if (winner == 1) p1.take(p1g, p2g)
      else p2.take(p2g, p1g)
    }

    result.winner = if (p1.hasNext) 1 else 2
    result.winner
  }

  override def calculate(): String = {
    val winner = playRecursiveGame(p1, p2)
    if (winner == 1) p1.score + ""
    else p2.score + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day22BS {
  def main(args: Array[String]): Unit = {
    new Day22BS().apply()
  }
}
