package challenge

import lib.Graphs

import scala.io.Source
import scala.util.matching.Regex

object Day07 {

  type Bags = Map[String, Map[String, Int]]

  def bagsWithin(name: String, bags: Bags): Int = {
    def acc(bag: String): Int = bags(bag).map(b => b._2 * acc(b._1)).sum + 1

    acc(name) - 1
  }

  val subPattern: Regex = """(\d+) ([\w ]+) bags?""".r
  def parseBag(s: String): (String, Int) = s match {
    case subPattern(n, bag) => bag -> n.toInt
  }
  def parseBags(s: String): Map[String, Int] = s match {
    case "no other bags" => Map.empty
    case s               => s.split(", ").map(parseBag).toMap
  }
  val pattern: Regex = """([\w ]+) bags contain (.*).""".r
  def parse(s: String): (String, Map[String, Int]) = s match {
    case pattern(color, bags) => color -> parseBags(bags)
  }

  val bags: Bags = Source.fromResource("day07.txt").getLines().map(parse).toMap

  def partOne(): Int =
    Graphs.DFS("shiny gold")(bag => bags.filter(_._2.contains(bag)).keys).size - 1

  def partTwo(): Int = bagsWithin("shiny gold", bags)

}
