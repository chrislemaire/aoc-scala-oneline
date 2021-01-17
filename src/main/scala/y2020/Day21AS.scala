package y2020

import java.nio.file.{Path, Paths}
import java.util.Scanner
import scala.io.Source

//noinspection SourceNotClosed
class Day21AS extends AdventOfCode with RegexParsers {
  private val f = "misc/src/main/resources/day21.txt"

  private case class Item(ingredients: Set[String], allergens: Set[String])

  private def ingredientsP: Parser[Set[String]] = "\\w+".r.+ ^^ { _.toSet }
  private def allergensP: Parser[Set[String]] = "(" ~> "contains" ~> rep1sep("\\w+".r, ",") <~ ")" ^^ { _.toSet }
  private def itemP: Parser[Item] = ingredientsP ~ allergensP ^^ { case i ~ a => Item(i, a) }

  private var items: List[Item] = Nil

  override def read(sc: Scanner): Unit = {
    items = Source.fromFile(f).getLines().map(parse(itemP, _).get).toList
  }

  override def calculate(): String = {
    val ingredients = items.flatMap(_.ingredients.toList)
    val allergens = items.flatMap(_.allergens).toSet

    var possible = allergens.map(a => a -> ingredients.toSet).toMap
    for (item <- items) {
      item.allergens.foreach(a => possible = possible.updated(a, possible(a).intersect(item.ingredients)))
    }

    var allergenIngredients: Set[String] = Set()

    var oldPossible: Map[String, Set[String]] = Map()
    var newPossible = possible
    while (oldPossible != newPossible) {
      oldPossible = newPossible
      newPossible.filter(_._2.size == 1)
        .foreach(a => {
          val chosen = a._2.head
          allergenIngredients += chosen
          newPossible = newPossible.filterKeys(_ != a._1)
            .mapValues(_.filterNot(_ == chosen))
        })
    }

    ingredients.filterNot(allergenIngredients).size + ""
  }

  override def file(): Path = Paths.get(f)
}

object Day21AS {
  def main(args: Array[String]): Unit = {
    new Day21AS().apply()
  }
}
