package cards.domain

import zio._
import GameConfig._

type GameId = Int



case class Table (
    console: Console, 
    deck: Deck, 
    game: GameConfig, 
    playersNumber: Int,
    playersRef: Ref[Chunk[Player]], 
    blindesRef: Ref[Blindes],
    gidRef: Ref[GameId],
    gameStageRef: Ref[GameStages],
    potRef: Ref[Int],
    actionsRef: Ref[Chunk[PlayerAction]]
  ) {

  def init(): ZIO[Random & Console, Nothing, Unit] = 
    for {
      random <- ZIO.service[Random]
      pid <- random.nextUUID
      players <- ZIO.foreach(Chunk.range(0, playersNumber))(n => Player(pid, n, n, this))
      _ <- playersRef.set(players)
    } yield ()

  def play(): ZIO[Console, Throwable, Unit] = 
    for {
      _ <- gidRef.update(_ + 1)
      _ <- deck.shuffle(game.cardsInDeck)
      players <- playersRef.get
      newPlayers <- ZIO.foreach((players.tail :+ players.head).zipWithIndexFrom(0))((p, i) => Player(p.pid, p.n, i, this))
      _ <- playersRef.set(newPlayers)
      _ <- ZIO.foreach(newPlayers)(p => p.addCard(deck.oneCard())).repeatN(game.cardsInHand-1)
      _ <- newPlayers(0).move()
    } yield ()

  def playN(gamesN: Int) = play().repeatN(gamesN - 1)

  def playerMove(pid: PlayerId, name: String, position: Int, action: PlayerActions, wager: Int) = 
    for {
      clock <- ZIO.service[Clock]
      _ <- potRef.update(_ + wager)
      stage <- gameStageRef.get
      dt <- clock.nanoTime
      //(pid: PlayerId, name: String, position: Int, gameStage: GameStages, action: PlayerActions, wager: Int, dt: Long)
      _ <- actionsRef.update(_ :+ PlayerAction(pid, name, position, stage, action, wager, dt))
    } yield ()

  def biggestMove() =
    for {
      actions <- actionsRef.get
      toClose = actions.maxBy(_.wager)
    } yield toClose

  def status(): ZIO[Console, Throwable, Unit] = 
    for {
      gid <- gidRef.get
      players <- playersRef.get
      blindes <- blindesRef.get
      _ <- console.printLine(s"[$gid] ${game.limit} ${game.name} ${blindes.sb}/${blindes.bb} for ${players.size} players: ")
      _ <-  ZIO.foreachDiscard(players)(p => p.status())
    } yield ()
}

object Table {
  def apply(game: GameConfig, playersNumber: Int, blindes: Blindes): ZIO[Console & Random, Throwable, Table] = {
    for {
      console <- ZIO.service[Console]
      deck <- Deck(game.cardsInDeck)
      playersRef <- Ref.make(Chunk.empty[Player])
      blindesRef <- Ref.make(blindes)
      gidRef <- Ref.make(-1)
      potRef <- Ref.make(0)
      actionsRef <- Ref.make(Chunk.empty[PlayerAction])
      gameStageRef <- Ref.make(GameStages.Preflop) // TODO: GameStages should be differ by games (fool?)
    } yield Table(console, deck, game, playersNumber, playersRef, blindesRef, gidRef, gameStageRef, potRef, actionsRef)
  }

}