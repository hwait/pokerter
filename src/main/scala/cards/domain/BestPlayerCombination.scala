package cards.domain

import zio._
import java.util.UUID

/** Best combination found in a player hand evaluation
 *
 *  @param comb the best combination found
 *  @param combNominals the nominals of combination parts sorted from high to low with the rest of hand nominals in descent order
 *  @param valued the hand valued as BestPlayerCombination
 *  @param cards whole list of cards from which the valued hand is chosen * 
 */

case class BestPlayerCombination(comb: PokerCombinations, combNominals: Chunk[Int], valued: Chunk[Card], cards: Chunk[Card]) {
  override def toString(): String = {

    val shows = valued.mkString("[",",","]")
    val all = cards.mkString("[",",","]")

    val combText = comb match 
      case PokerCombinations.Nothing => comb.name
      case PokerCombinations.HighCard => 
        comb.pattern.format(nomToStr(combNominals.head), combNominals.tail.map(nomToStr).mkString(","))
      case PokerCombinations.Pair => 
        comb.pattern.format(nomToStr(combNominals.head), combNominals.tail.map(nomToStr).mkString(","))
      case PokerCombinations.TwoPairs =>
        comb.pattern.format(nomToStr(combNominals.head), nomToStr(combNominals.tail.head), nomToStr(combNominals.last))
      case PokerCombinations.Set =>
        comb.pattern.format(nomToStr(combNominals.head), combNominals.tail.map(nomToStr).mkString(","))
      case PokerCombinations.Straight =>
        comb.pattern.format(nomToStr(combNominals.head))
      case PokerCombinations.Flush =>
        comb.pattern.format(nomToStr(combNominals.head))
      case PokerCombinations.FullHouse =>
        comb.pattern.format(nomToStr(combNominals.head), nomToStr(combNominals.last))
      case PokerCombinations.Quads =>
        comb.pattern.format(nomToStr(combNominals.head))
      case PokerCombinations.StraightFlush =>
        comb.pattern.format(nomToStr(combNominals.head))
      case PokerCombinations.RoyalFlush => comb.name

    s" has $combText, shows $shows from $all"
  }
}

object BestPlayerCombination {
  def apply(): BestPlayerCombination = BestPlayerCombination(PokerCombinations.Nothing, Chunk.empty[Int], Chunk.empty[Card], Chunk.empty[Card])
}
