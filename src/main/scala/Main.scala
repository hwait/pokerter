import zio._
import cards.domain._

object Main extends ZIOAppDefault {
  def table(
      game: GameConfig,
      playerNumbers: Int,
      blindes: Blindes
  ): ZIO[Random & Console, Throwable, Table] =
    for {
      table <- Table(game, playerNumbers, blindes)
      _ <- table.init()
    } yield table

  override def run =
    (for {
      table1 <- table(GameConfig.NLHoldem, 6, Blindes(50, 100, 0))
      _ <- table1.playN(2)
    } yield ())
      .provide(
        Clock.live,
        Logger.live,
        Console.live,
        Random.live
      )
}
