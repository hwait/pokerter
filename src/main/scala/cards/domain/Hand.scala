package cards.domain

final case class Hand(cards: List[Card]) {
  def add(others: List[Card]): Hand = Hand(cards ++ others)
  def add(another: Card): Hand = Hand(cards :+ another)
  val stage: Int = cards.length
  val evaluate =
    // if (cards.length == 5)
    cards.foldLeft(0)((s, c) => s + c.nominal)
  // else 0
  override def toString(): String = 
    cards.mkString
}
object Hand {
  def apply(a: Card, b: Card): Hand = Hand(List(a, b))
}
