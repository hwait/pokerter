package cards.domain

enum GameResults(val n: Int):
  case Lost extends GameResults(0)
  case LostSidePot extends GameResults(1)
  case Tied extends GameResults(2)
  case Won extends GameResults(3)
  case WonSidePot extends GameResults(4)
end GameResults