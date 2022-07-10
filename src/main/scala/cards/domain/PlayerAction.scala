package cards.domain

import zio._
import java.util.UUID

/** For logging player's actions
 *  Extends trait GameActions
 * 
 *  @param pid player ID
 *  @param name player name
 *  @param position player position on the table
 *  @param gameStage game stage from enum
 *  @param action player action from enum
 *  @param wager player wager in cents
 *  @param bank player bank in cents
 *  @param dt event time in nanoseconds
 */
case class PlayerAction(
  pid: PlayerId,
  name: String,
  position: Int,
  gameStage: GameStages,
  action: PlayerActions,
  wager: Int,
  bank: Int,
  dt: Long
) extends GameActions {
  override def toString(): String = {
    val bet = if (wager > 0) s" on $wager, bank $bank" else ""
    s"$gameStage: [$pid: $name] on $position did $action$bet at $dt"
  }
}
