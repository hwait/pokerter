package cards.domain

  
final case class Card(rank: Int) {
  val nominal = rank%13
  val suit = rank/13

  val nominalString = nominal match
    case nom if nom > -1 && nom < 8 => (nom+2).toString()
    case 8 => "T"
    case 9 => "J"
    case 10 => "Q"
    case 11 => "K"
    case 12 => "A"
    case _ => ""

  val suitString = suit match 
    case 0 => Console.GREEN + "\u2663" + Console.RESET
    case 1 => Console.BLUE + "\u2666" + Console.RESET
    case 2 => Console.RED + "\u2665" + Console.RESET
    case 3 => Console.BLACK + "\u2660" + Console.RESET

  override def toString(): String = 
    nominalString+suitString
}

  
