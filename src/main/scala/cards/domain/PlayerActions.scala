package cards.domain

enum PlayerActions(val name: String, val aggressive: Boolean):
  case Fold extends PlayerActions("Fold", false)
  case FoldDisconnected extends PlayerActions("FoldDisconnected", false)
  case Check extends PlayerActions("Check", false)
  case CheckDisconnected extends PlayerActions("CheckDisconnected", false)
  case Call extends PlayerActions("Call", false)
  case PostSB extends PlayerActions("PostSB", false)
  case PostBB extends PlayerActions("PostBB", false)
  case Bet extends PlayerActions("Bet", true)
  case Raise extends PlayerActions("Raise", true)
end PlayerActions