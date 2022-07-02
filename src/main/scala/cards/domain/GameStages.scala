package cards.domain

enum GameStages(val n: Int):
  case Preflop extends GameStages(0)
  case Flop extends GameStages(1)
  case Turn extends GameStages(2)
  case River extends GameStages(3)
end GameStages