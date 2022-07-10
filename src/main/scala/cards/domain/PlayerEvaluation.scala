package cards.domain

import zio._
import java.util.UUID
/** For logging player's evaluation
 *  Extends trait GameActions
 * 
 *  @param pid player ID
 *  @param name player name
 *  @param holeCards player hole cards
 *  @param result game result from enum
 *  @param comb best combination found via evaluation
 *  @param profit player profit or loss in cents
 *  @param bank player bank after the play in cents
 *  @param dt event time in nanoseconds
 */
case class PlayerEvaluation(
  pid: PlayerId,
  name: String,
  holeCards: Chunk[Card],
  result: GameResults,
  comb: BestPlayerCombination,
  profit: Int,
  bank: Int,
  dt: Long
) extends GameActions {
  override def toString(): String = {
    s"[$pid: $name] $result $profit => $bank with ${holeCards.mkString("[",",","]")}$comb"
  }
}
