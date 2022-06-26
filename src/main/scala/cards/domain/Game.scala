package cards.domain

import zio._
import GameConfig._

case class Game(game: GameConfig, limit: String, sb: Int, bb: Int, players: Chunk[Player]) {
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

object Game {
  def init(game: GameConfig, playersNumber: Int, limit: String, sb: Int, bb: Int): ZIO[Deck & (Random & Console), Throwable, Game] = 
    for {
      players <- ZIO.foreach(Chunk.range(0, playersNumber))(n => Player.init(n))
    } yield Game(game, limit, sb, bb, players)
  
}