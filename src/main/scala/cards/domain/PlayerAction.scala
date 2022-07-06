package cards.domain

import zio._
import java.util.UUID

case class PlayerAction(pid: PlayerId, name: String, position: Int, gameStage: GameStages, action: PlayerActions, wager: Int, dt: Long) extends GameActions {
  override def toString(): String = {
    val bet = if (wager>0) s" on $wager" else ""
    s"$gameStage: [$pid: $name] on $position did $action$bet at $dt"
  }
}