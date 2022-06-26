package cards.domain

import zio._

case class Table (game: Game) {
  def playN(n: Int) = for {
    _ <- ZIO.foreach(Chunk.range(0, n))(_ => game.playGame() *> game.status())
  } yield ()
}

object Table {
  def init(gameConfig: GameConfig, playersNumber: Int, limit: String, sb: Int, bb: Int): ZIO[Deck & (Random & Console), Throwable, Table] = {
    for {
      game <- Game.init(gameConfig, playersNumber, limit, sb, bb)
      // _ <- game.status()
    } yield Table(game)
  }
}