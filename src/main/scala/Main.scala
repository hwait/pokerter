import zio._
import cards.domain._
import cards.logic.Table
import cards.services.Logger

object Main extends ZIOAppDefault {
  def table(
    tableId: Int,
    game: GameConfigs,
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
      _      <- ZIO.foreachPar(Range(0, 1))(i => table(i, GameConfigs.NLHoldem, 6, Blindes(500, 1000, 0), 1))
      logger <- ZIO.service[Logger]
      _      <- logger.saveAndClear()
    } yield ())
      .provide(
        Clock.live,
        Logger.live,
        Console.live,
        Random.live
      )
}
