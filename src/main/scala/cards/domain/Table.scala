package cards.domain

import zio._

case class Table (dealer: Dealer) {
  def playN(n: Int) = for {
    _ <- ZIO.foreach(Chunk.range(0, n))(_ => dealer.playGame()/* *> dealer.status()*/)
  } yield ()
}

object Table {
  def init(gameConfig: GameConfig, playersNumber: Int, blindes: Blindes) = {
    for {
      dealer <- ZIO.service[Dealer]
      _ <- dealer.init(gameConfig, playersNumber, blindes)
      // _ <- game.status()
    } yield Table(dealer)
  }
}