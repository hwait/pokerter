package cards.domain

enum GameConfig(val name: String, val cardsInDeck: Int, val cardsInHand: Int, val limit: String):
  case NLHoldem extends GameConfig("Holdem", 52, 2, "No")
  case FLHoldem extends GameConfig("Holdem", 52, 2, "Fixed")
  case PLOmaha extends GameConfig("Omaha", 52, 4, "Pot")
  case NLOmaha extends GameConfig("Omaha", 52, 4, "No")
  case Fool extends GameConfig("Fool", 36, 6, "")
end GameConfig