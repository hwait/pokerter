package cards.domain

import zio._
import java.util.UUID

case class DealerAction(action: DealerActions, cards: Chunk[Card], dt: Long) extends GameActions {
  override def toString(): String = {
    val cardsText = cards.mkString("[","","]")
    s"DEALER: deals $cardsText at $dt"
  }
}