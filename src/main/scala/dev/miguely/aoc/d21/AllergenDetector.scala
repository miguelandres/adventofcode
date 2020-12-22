package dev.miguely.aoc.d21

import scala.collection.mutable.{HashMap, HashSet, MultiMap}
import scala.io.Source
import scala.util.control.Breaks._

object AllergenDetector {

  type MSet[T] = scala.collection.mutable.Set[T]
  type StringSet = Set[String]
  type DoubleStringSet = Set[StringSet]
  type MStringSet = MSet[String]
  type MDoubleStringSet = MSet[StringSet]
  type StringDoubleSetMap = HashMap[String, DoubleStringSet]
  type StringSetMap = HashMap[String, StringSet]
  type MStringDoubleSetMap = HashMap[String, MDoubleStringSet]
  type MStringSetMap = HashMap[String, MStringSet]
  type StringMultiMap = MultiMap[String, String]
  type StringSetMultiMap = MultiMap[String, StringSet]

  val linput = "input/d21.txt"
  val sinput = "input/small/d21.txt"
  val input = linput

  val allIngredients = new HashSet[String]()
  val allAllergens = new HashSet[String]()
  val ingredientsToRecipes = new MStringDoubleSetMap with StringSetMultiMap()
  val ingredientsToAllergenLists = new MStringDoubleSetMap with StringSetMultiMap()
  val allergensToRecipes = new MStringDoubleSetMap with StringSetMultiMap()
  val allergensToPossibleIngredients = new MStringSetMap with StringMultiMap()
  val ingredientsToPossibleAllergens = new MStringSetMap with StringMultiMap()
  val fixedIngredients = new HashSet[String]()

  def main(args: Array[String]): Unit = {
    for (line <- Source.fromFile(input).getLines()) {
      var recipe: StringSet = null
      var allergens: StringSet = null
      parseLine(line) match {
        case (r, a) => recipe = r; allergens = a;
      }
      for (ingredient <- recipe) {
        ingredientsToAllergenLists.addBinding(ingredient, allergens)
        ingredientsToRecipes.addBinding(ingredient, recipe)
        allergens.foreach(ingredientsToPossibleAllergens.addBinding(ingredient, _))
        allIngredients.add(ingredient)
      }
      for (allergen <- allergens) {
        allergensToRecipes.addBinding(allergen, recipe)
        recipe.foreach(allergensToPossibleIngredients.addBinding(allergen, _))
        allAllergens.add(allergen)
      }
    }

    for (allergen <- allAllergens)
      for (ingredient <- allergensToPossibleIngredients(allergen))
        if (!(allergensToRecipes(allergen).forall(_.contains(ingredient)))) {
          allergensToPossibleIngredients.removeBinding(allergen, ingredient)
          ingredientsToPossibleAllergens.removeBinding(ingredient, allergen)
        }

    println(allergensToPossibleIngredients)
    println(ingredientsToPossibleAllergens)
    var somethingToFix = true
    while (somethingToFix) {
      somethingToFix = false
      for (ingredient <- allIngredients) {
        var possibleAllergens =
          ingredientsToPossibleAllergens.getOrElse(ingredient, new HashSet[String])
        if (possibleAllergens.size == 1 && !fixedIngredients.contains(ingredient)) {
          somethingToFix = true
          fixedIngredients.add(ingredient)
          var allergen = possibleAllergens.iterator.next
          for (formerIngredient <- allergensToPossibleIngredients(allergen))
            if (formerIngredient != ingredient) {
              allergensToPossibleIngredients.removeBinding(allergen, formerIngredient)
              ingredientsToPossibleAllergens.removeBinding(formerIngredient, allergen)
            }
        }
      }
    }

    for (allergen <- allAllergens)
      println(
        s"$allergen: " +
          allergensToPossibleIngredients
            .getOrElse(allergen, new HashSet[String])
            .addString(new StringBuilder, ",")
      )
    println(ingredientsToPossibleAllergens)

    var safeIngredients = allIngredients -- ingredientsToPossibleAllergens.keySet
    println(safeIngredients.addString(new StringBuilder("safe: "), ","))
    var count = 0
    for (ingredient <- safeIngredients)
      for (recipes <- ingredientsToRecipes(ingredient)) count = count + 1

    println(count)
  }

  def parseLine(line: String): (StringSet, StringSet) = {
    var parts = line.split(""" \(contains """)
    var recipeSet = parts(0).split(" ").toSet
    var allergenSet = parts(1).slice(0, parts(1).size - 1).split(", *").toSet
    return (recipeSet, allergenSet)
  }

}
