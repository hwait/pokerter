import zio._
import cards.domain._

object Main extends ZIOAppDefault {
  override def run =
    (for {
      table <- Table.init(GameConfig.NLHoldem, 6, Blindes(50, 100, 0))
      _ <- table.playN(1)
      // table <- Table.init(GameConfig.Omaha, 4, "PL", 50, 100)
      // _ <- table.playN(2)
    } yield ())
      .provide(
        Clock.live,
        Logger.live,
        Console.live,
        Random.live,
        Dealer.live,
        Deck.live
      )
}
