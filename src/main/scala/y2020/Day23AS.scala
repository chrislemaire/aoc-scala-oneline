package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner

//noinspection SourceNotClosed
class Day23AS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day23.txt"

  private case class Cup(id: Int,
                         var next: Cup = null) {
    def nextThree: List[Int] = List(next.id, next.next.id, next.next.next.id)
    def moveNextThree(dest: Cup) = {
      val newNext = next.next.next.next

      next.next.next.next = dest.next
      dest.next = next

      next = newNext
    }
  }

  private var n: Int = 0
  private var selected: Cup = null
  private var cups: Map[Int, Cup] = Map()

  override def read(sc: Scanner): Unit = {
    val c = sc.nextLine().toCharArray.map(_.asDigit).toList
    cups = c.sorted.map(id => id -> Cup(id)).toMap
    n = cups.size

    cups.values.foreach(cup => {
      cup.next = cups(c((c.indexOf(cup.id) + 1) % n))
    })
    selected = cups(c.head)
  }

  override def calculate(): String = {
    for (_ <- 1 to 100) {
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

    selected = cups(1).next
    var cupsInOrder = Vector[Cup]()
    var ids = Set[Int](1)
    while (!ids(selected.id)) {
      cupsInOrder = cupsInOrder :+ selected
      ids += selected.id
      selected = selected.next
    }

    cupsInOrder.map(_.id.toString).mkString("")
  }

  override def file(): Path = Paths.get(f)
}

object Day23AS {
  def main(args: Array[String]): Unit = {
    new Day23AS().apply()
  }
}
