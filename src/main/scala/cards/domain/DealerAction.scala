package cards.domain

import zio._
import java.util.UUID

/** For logging dealer's actions
 *  Extends trait GameActions
 * 
 *  @param action dealer actions from enum 
 *  @param cards cards on the board
 *  @param dt event time in nanoseconds
 */
case class DealerAction(action: DealerActions, cards: Chunk[Card], dt: Long) extends GameActions {
  override def toString(): String = {
    val cardsText = cards.mkString("[","","]")
    s"DEALER: deals $cardsText at $dt"
  }
}