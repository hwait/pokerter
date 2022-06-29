package cards.domain

import zio._
import GameConfig._

case class Dealer(game: GameConfig, limit: String, sb: Int, bb: Int, players: Chunk[Player]) {
  def playGame() = 
    for {
      deck <- ZIO.service[Deck]
      _ <- deck.shuffle(game.cardsInDeck)
      _ <- ZIO.foreach(players)(p => p.renew(players.size-1))
      _ <- ZIO.foreach(players)(p => p.addCard(deck.oneCard())).repeatN(game.cardsInHand-1)
    } yield ()

  def status(): ZIO[Console, Throwable, Unit] = 
    for {
      console <- ZIO.service[Console]
      _ <- console.printLine(s"[$limit ${game.name} $sb/$bb for ${players.size} players]: ")
      _ <-  ZIO.foreach(players)(p => p.status())
    } yield ()
}

object Dealer {
  def init(game: GameConfig, playersNumber: Int, limit: String, sb: Int, bb: Int): ZIO[Deck & (Random & Console), Throwable, Dealer] = 
    for {
      random <- ZIO.service[Random]
      pid <- random.nextUUID
      players <- ZIO.foreach(Chunk.range(0, playersNumber))(n => Player.init(pid, n))
    } yield Dealer(game, limit, sb, bb, players)
  
}