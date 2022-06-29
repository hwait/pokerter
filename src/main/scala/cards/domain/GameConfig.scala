package cards.domain

enum GameConfig(val name: String, val cardsInDeck: Int, val cardsInHand: Int):
  case Holdem extends GameConfig("Holdem", 52, 2)
  case Omaha extends GameConfig("Omaha", 52, 4)
  case Fool extends GameConfig("Fool", 36, 6)
end GameConfig