package cards.domain
/*
enum Color(val rgb: Int):
  case Red   extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue  extends Color(0x0000FF)
*/
enum GameConfig(val name: String, val cardsInDeck: Int, val cardsInHand: Int):
  case Holdem extends GameConfig("Holdem", 52, 2)
  case Omaha extends GameConfig("Omaha", 52, 4)
  case Fool extends GameConfig("Fool", 36, 6)
end GameConfig