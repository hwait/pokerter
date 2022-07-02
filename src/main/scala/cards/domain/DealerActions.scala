package cards.domain

enum DealerActions(val name: String):
  case DealsFlop extends DealerActions("DealsFlop")
  case DealsTurn extends DealerActions("DealsTurn")
  case DealsRiver extends DealerActions("DealsRiver")
end DealerActions