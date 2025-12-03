package challenge

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day21 {

  type Allergen   = String
  type Ingredient = String
  type Food       = (Set[Ingredient], Set[Allergen])
  type Causes     = Map[Allergen, Ingredient]

  val pattern: Regex = """(.+) \(contains (.+)\)""".r
  def parse(s: String): Food = s match {
    case pattern(ingredients, allergens) =>
      (ingredients.split(" ").toSet, allergens.split(", ").toSet)
  }

  def nonAllergenicIngredients(): Set[Ingredient] = {
    val map         = allergens.map(a => a -> foods.filter(_._2.contains(a)).map(_._1).reduce(_ & _)).toMap
    val ingredients = map.values.reduce(_ | _)
    foods.flatMap(_._1).toSet -- ingredients
  }

  def allergenicIngredients(): Causes = {
    val map = allergens
      .map(
        a =>
          a -> foods
            .collect { case (is, as) if as.contains(a) => is -- safeIngredients }
            .reduce(_ & _))
      .toMap
      .filter(kv => kv._2.nonEmpty)

    @tailrec
    def helper(xs: Map[Allergen, Set[Ingredient]], acc: Causes): Causes = {
      if (xs.isEmpty) acc
      else {
        val matching: Map[Allergen, Ingredient] = xs.collect {
          case (k, v) if v.size == 1 => k -> v.iterator.next()
        }
        val next = xs.view.mapValues(_ -- matching.values).filter(_._2.nonEmpty).toMap
        helper(next, acc ++ matching)
      }
    }

    helper(map, Map.empty)
  }

  val foods: List[Food]                = Source.fromResource("day21.txt").getLines().map(parse).toList
  val allergens: Set[Allergen]         = foods.flatMap(_._2).toSet
  val safeIngredients: Set[Ingredient] = nonAllergenicIngredients()

  def partOne(): Int    = foods.map(_._1.count(safeIngredients.contains)).sum
  def partTwo(): String = allergenicIngredients().toSeq.sortBy(_._1).map(_._2).mkString(",")

}
