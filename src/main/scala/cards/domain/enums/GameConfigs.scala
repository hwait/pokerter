package cards.domain

enum GameConfigs(val name: String, val cardsInDeck: Int, val cardsInHand: Int, val limit: String):
  case NLHoldem extends GameConfigs("Holdem", 52, 2, "No limit")
  case FLHoldem extends GameConfigs("Holdem", 52, 2, "Fixed limit")
  case PLOmaha  extends GameConfigs("Omaha", 52, 4, "Pot limit")
  case NLOmaha  extends GameConfigs("Omaha", 52, 4, "No limit")
  case Fool     extends GameConfigs("Fool", 36, 6, "")
end GameConfigs
