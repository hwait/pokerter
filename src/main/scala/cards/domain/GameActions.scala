package cards.domain

import zio._
import java.util.UUID

trait GameActions



case class PlayerAction(pid: PlayerId, name: String, position: Int, gameStage: GameStages, action: PlayerActions, wager: Int, dt: Long) extends GameActions {
  override def toString(): String = {
    val bet = if (wager>0) s" on $wager" else ""
    s"$gameStage: [$pid: $name] on $position did $action$bet at $dt"
  }
}

case class DealerAction(action: DealerActions, cards: Chunk[Card], dt: Long) extends GameActions {
  override def toString(): String = {
    val cardsText = cards.mkString("[","","]")
    s"DEALER: deals $cardsText at $dt"
  }
}

case class PlayerEvaluation(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int, dt: Long) extends GameActions {
  override def toString(): String = {
    val cardsText = cards.mkString("[","","]")
    s"[$pid] $result $profit with $topCombination $cardsText"
  }
}