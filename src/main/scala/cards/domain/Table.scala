package cards.domain

import zio._
import GameConfig._

type TableId = Int

case class Table(
    tableId: TableId,
    gameConfig: GameConfig,
    gameRef: Ref[Game],
    playersN: Int,
    playersRef: Ref[Chunk[Player]],
    deck: Deck,
    console: Console
) {

  def playRound(id: Int): ZIO[Logger & Clock & Console, Throwable, Unit] = 
    for {
      game <- gameRef.get
      players <- playersRef.get
      status <- players(id).statusRef.get
      _ <- ZIO.when(status == PlayerStatus.Active)(game.playerMove(players(id)))
      isNextRound <- game.isNextRound
      _ <- if (isNextRound) playRound(if (id>=playersN-1) 0 else id + 1)
      else game.nextRound()
    } yield ()

  val play =
    for {
      logger <- ZIO.service[Logger]
      game <- gameRef.get
      newGame <- Game(tableId, game.gid+1, gameConfig, playersN, game.blindes)
      _ <- gameRef.set(newGame)
      _ <- deck.shuffle
      players <- playersRef.get
      // pid: PlayerId, n: Int, position: Int, status: PlayerStatus, bank: Int
      _ <- ZIO.foreach(players)(p => p.addCard(deck.oneCard)).repeatN(gameConfig.cardsInHand - 1)
      _ <- status
      _ <- playRound(0)
      _ <- playRound(0)
      _ <- playRound(0)
      _ <- playRound(0)
      newPlayers <- ZIO.foreach((players.last +: players.dropRight(1)).zipWithIndexFrom(0))((p, i) => p.renew *> ZIO.succeed(p.copy(position = i)))
      _ <- playersRef.set(newPlayers)
    } yield ()

  def playN(gamesN: Int) = play.repeatN(gamesN - 1)

  val status = 
    for {
      game <- gameRef.get
      players <- playersRef.get
      _ <- console.printLine(s"[T$tableId / G${game.gid}] ${gameConfig.limit} ${gameConfig.name} ${game.blindes.sb}/${game.blindes.bb} for $playersN pls:")
      _ <- ZIO.foreach(players)(p => p.status)
    } yield ()
}


object Table {
  def apply(tableId: TableId, blindes: Blindes, gameConfig: GameConfig, playersN: Int): ZIO[Logger & Console & Random, Throwable, Table] = {
    for {
      console <- ZIO.service[Console]
      game <- Game(tableId, 0, gameConfig, playersN, blindes)

      gameRef <- Ref.make(game)

      players <- ZIO.foreach(Chunk.range(0, playersN)) { n => 
        Player(tableId*10+n, n, n, PlayerStatus.Active, blindes.bb * 100)
      }

      playersRef <- Ref.make(players)
      deck <- Deck(gameConfig.cardsInDeck)
    } yield Table( tableId, gameConfig, gameRef, playersN, playersRef, deck, console )
  }
}
