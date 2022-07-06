import zio._
import cards.domain._

object Main extends ZIOAppDefault {
  def table(
    tableId: Int,
    game: GameConfig,
    playerNumbers: Int,
    blindes: Blindes,
    n: Int
  ) =
    for {
      table <- Table(tableId, blindes, game, playerNumbers)
      _     <- table.playN(n)
    } yield table

  override def run =
    (for {
      _ <- ZIO.foreachPar(Range(0, 10))(i => table(i, GameConfig.NLHoldem, 6, Blindes(500, 1000, 0), 10))
    } yield ())
      .provide(
        Clock.live,
        Logger.live,
        Console.live,
        Random.live
      )
}
