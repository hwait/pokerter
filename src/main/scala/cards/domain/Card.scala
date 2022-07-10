package cards.domain
/** Card representation
 *
 *  @param rank the card number in 52-cards deck
 *  TODO: What about games with another cards count?
 */
final case class Card(rank: Int) {
  val nominal: Int = rank%13
  val suit: Int = rank/13

  val nominalString: String = nomToStr(nominal)

  val suitString: String = suit match // 4-colors deck
    case 0 => Console.GREEN + "\u2663" + Console.RESET
    case 1 => Console.BLUE + "\u2666" + Console.RESET
    case 2 => Console.RED + "\u2665" + Console.RESET
    case 3 => Console.BLACK + "\u2660" + Console.RESET

  override def toString(): String = 
    nominalString+suitString
}

  
