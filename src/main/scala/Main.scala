import zio._
import cards.domain._

object Main extends ZIOAppDefault {
  override def run =
    (for {
      table <- Table.init(GameConfig.Holdem, 6, "NL", 50, 100)
      _ <- table.playN(2)
      table <- Table.init(GameConfig.Omaha, 4, "PL", 50, 100)
      _ <- table.playN(2)
    } yield ())
      .provide(
        Clock.live,
        Logger.live,
        Console.live,
        Random.live,
        Deck.live
      )
}
