package cards.domain

enum PokerCombinations(val name: String, val pattern: String):
  case Nothing extends PokerCombinations("No combination yet", "No combination yet")
  case HighCard extends PokerCombinations("High card", "High card: %s with kickers %s")
  case Pair extends PokerCombinations("Pair", "Pair of %ss with kickers %s")
  case TwoPairs extends PokerCombinations("Two pairs", "Two pairs: %ss and %ss with kicker %s")
  case Set extends PokerCombinations("Set", "Set of %ss with kickers %s")
  case Straight extends PokerCombinations("Straight", "Straight %s high")
  case Flush extends PokerCombinations("Flush", "Flush %s high")
  case FullHouse extends PokerCombinations("Full house", "Full house (%ss full of %ss)")
  case Quads extends PokerCombinations("Four of a kind (quads)", "Four of a kind (%ss)")
  case StraightFlush extends PokerCombinations("Straight flush", "Straight flush %s high")
  case RoyalFlush extends PokerCombinations("Royal flush", "Royal flush")
end PokerCombinations