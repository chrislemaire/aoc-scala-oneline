package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner

//noinspection SourceNotClosed
class Day23BS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day23.txt"

  private case class Cup(id: Long,
                         var next: Cup = null) {
    def nextThree: List[Long] = List(next.id, next.next.id, next.next.next.id)
    def moveNextThree(dest: Cup) = {
      val newNext = next.next.next.next

      next.next.next.next = dest.next
      dest.next = next

      next = newNext
    }
  }

  private var n: Long = 0L
  private var selected: Cup = null
  private var cups: Map[Long, Cup] = Map()

  override def read(sc: Scanner): Unit = {
    n = 1000000
    var c = sc.nextLine().toCharArray.map(_.asDigit.toLong).toList
    c = c ++ (c.max + 1 to n)

    cups = c.map(id => id -> Cup(id)).toMap

    c.zip(c.tail ++ List(c.head)).foreach(t =>
      cups(t._1).next = cups(t._2))
    selected = cups(c.head)
  }

  override def calculate(): String = {
    for (_ <- 1 to 10000000) {
      // Calculate the actually picked characters
      val picked = selected.nextThree

      // Find destination cup
      var destination: Cup = null
      for (i <- 1 to 4) {
        val possible = (selected.id - i + n - 1) % n + 1
        if (!picked.contains(possible) && destination == null) {
          destination = cups(possible)
        }
      }

      // Add back the removed cups after destination cup
      selected.moveNextThree(destination)

      selected = selected.next
    }

    (cups(1).next.id * cups(1).next.next.id) + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day23BS {
  def main(args: Array[String]): Unit = {
    new Day23BS().apply()
  }
}
