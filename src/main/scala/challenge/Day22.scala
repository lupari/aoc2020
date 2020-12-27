package challenge

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day22 {

  type Deck  = Queue[Int]
  type Decks = (Deck, Deck)

  def score(deck: Deck): Int = deck.reverse.zipWithIndex.map(c => (c._2 + 1) * c._1).sum

  @tailrec
  def game1(decks: Decks): (Int, Deck) = decks match {
    case (d1, d2) if d2.isEmpty => (1, d1)
    case (d1, d2) if d1.isEmpty => (2, d2)
    case _ =>
      val ((c1, d1), (c2, d2)) = (decks._1.dequeue, decks._2.dequeue)
      if (c1 > c2) game1(d1 :+ c1 :+ c2, d2) else game1(d1, d2 :+ c2 :+ c1)
  }

  implicit val memo: Set[Decks] = Set.empty
  def game2(decks: Decks)(implicit memo: Set[Decks]): (Int, Deck) = decks match {
    case d if memo.contains(d)  => (1, d._1)
    case (d1, d2) if d2.isEmpty => (1, d1)
    case (d1, d2) if d1.isEmpty => (2, d2)
    case _ =>
      val ((c1, d1), (c2, d2)) = (decks._1.dequeue, decks._2.dequeue)
      val newDeal =
        if (c1 <= d1.size && c2 <= d2.size) { // sub-game
          val winner = game2((d1.take(c1), d2.take(c2)))._1
          if (winner == 1) (d1 :+ c1 :+ c2, d2) else (d1, d2 :+ c2 :+ c1)
        } else {
          if (c1 > c2) (d1 :+ c1 :+ c2, d2) else (d1, d2 :+ c2 :+ c1)
        }
      game2(newDeal)(memo + decks)
  }

  def play()(gf: Decks => (Int, Deck)): Deck = gf((Queue() ++ deck1, Queue() ++ deck2))._2

  def parse(s: String): List[Int] = s.linesIterator.drop(1).map(_.toInt).toList

  val input: Seq[String] = Source.fromResource("day22.txt").mkString.split("\n\n").toList
  val (deck1, deck2)     = (parse(input.head), parse(input.last))

  def partOne(): Int = score(play()(game1))
  def partTwo(): Int = score(play()(game2))

}
