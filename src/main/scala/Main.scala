import zio._
import cards.domain._

object Main extends ZIOAppDefault {
  override def run =
    (for {
      _ <- Table
        .init(GameConfig.NLHoldem, 6, Blindes(50, 100, 0))
        .flatMap(t => t.playN(2))
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
