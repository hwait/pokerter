package cards.domain
/**
 * Tied is questionable, if two players ties in heads-up it can be treated ad both Won with pot splitting
*/
enum GameResults:
  case Lost, LostSidePot, Tied, Won, WonSidePot