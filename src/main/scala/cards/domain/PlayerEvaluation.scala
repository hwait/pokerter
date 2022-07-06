package cards.domain

import zio._
import java.util.UUID

case class PlayerEvaluation(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int, dt: Long) extends GameActions {
  override def toString(): String = {
    val cardsText = cards.mkString("[","","]")
    s"[$pid] $result $profit with $topCombination $cardsText"
  }
}