package cards

package object domain {
  
  /** For logging different acions in the Logger service */
  trait GameActions

  /** Player Id is Int for now */
  type PlayerId = Int
  
  /** Game Id is Int for now */
  type GameId = Int
  
  /** Table Id is Int for now */
  type TableId = Int
  
  /** Represents Card nominal as string */
  def nomToStr(nominal: Int): String = nominal match
    case nom if nom > -1 && nom < 8 => (nom+2).toString()
    case 8 => "T"
    case 9 => "J"
    case 10 => "Q"
    case 11 => "K"
    case 12 => "A"
    case _ => ""
}
