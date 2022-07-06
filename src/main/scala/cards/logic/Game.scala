package cards.logic
import zio._
// import GameConfig._
import java.io.IOException
import cards.domain._
import cards.services.Logger

type GameId = Int

case class Game private (
    tid: TableId,
    gid: GameId,
    gameConfig: GameConfig,
    blindes: Blindes,
    gameStageRef: Ref[Int],
    playersNumber: Int,
    currentPlayersNumberRef: Ref[Int],
    potRef: Ref[Int],
    biggestWagerRef: Ref[Int],
    playersConsensusNumberRef: Ref[Int],
    actionsRef: Ref[Chunk[PlayerAction]],
    logger: Logger,
    console: Console
) {

  def playerMove(player: Player) = 
    for {
      logger <- ZIO.service[Logger]
      gameStage <- gameStageRef.get
      biggestWager <- biggestWagerRef.get
      actions <- actionsRef.get
      pa <- player.move(gameStage, actions, biggestWager, blindes)
      _ <- actionsRef.update(_ :+ pa)
      _ <- potRef.update(_ + pa.wager)
      _ <- logger.playerMoved(pa)
      _ <- if (pa.action == PlayerActions.Fold || pa.action == PlayerActions.FoldDisconnected) 
          currentPlayersNumberRef.update(_ - 1) *> actionsRef.update(_.filterNot(a => a.pid == pa.pid))
        else if (pa.action.aggressive) // If there is an aggressive move other players should make their moves. Also update the biggest wager
          playersConsensusNumberRef.update(_ => 0) *> biggestWagerRef.set(pa.wager)
        else
          playersConsensusNumberRef.update(_ + 1) // otherwise move step toward next round
    } yield ()
  
  val isNextRound: Task[Boolean] = for {
    currentPlayersN <- currentPlayersNumberRef.get
    playersConsensus <- playersConsensusNumberRef.get
  } yield playersConsensus < currentPlayersN
  
  def nextRound(): ZIO[Console, Throwable, Unit] = 
    for {
      _ <- status
      _ <- gameStageRef.update(_ + 1)
      _ <- biggestWagerRef.set(0)
      _ <- actionsRef.set(Chunk.empty[PlayerAction])
      _ <- playersConsensusNumberRef.set(0)
      // currentPlayersN <- currentPlayersNRef.get
      // playersConsensus <- playersConsensusRef.get
      // bw <- biggestWagerRef.get
      // actions <- actionsRef.get
      // _ <- console.printLine((currentPlayersN, playersConsensus, bw, actions))      
    } yield ()

  val status: ZIO[Console, Throwable, Unit] =
    for {
      gameStage <- gameStageRef.get
      currentPlayersNumber <- currentPlayersNumberRef.get
      pot <- potRef.get
      biggestWager <- biggestWagerRef.get
      playersConsensusNumber <- playersConsensusNumberRef.get
      actions <- actionsRef.get
      _ <- ZIO.foreach(actions)(a => console.printLine(a))
      _ <- console.printLine(s"[T$tid / G$gid] Pot ${pot} biggestWager ${biggestWager}, consensus ${playersConsensusNumber}/${currentPlayersNumber}.")
    } yield ()
}

object Game {
  /*
  tid: TableId,
      gid: GameId,
      gameConfig: GameConfig,
      blindesRef: Ref[Blindes],
      gameStageRef: Ref[Int],
      playersNumber: Int,
      currentPlayersNumberRef: Ref[Int],
      potRef: Ref[Int],
      biggestWagerRef: Ref[Int],
      playersConsensusNumberRef: Ref[Int],
      actionsRef: Ref[Chunk[PlayerAction]],
      logger: Logger,
      console: Console
  */
  def apply(tid: TableId, gid: GameId, gameConfig: GameConfig, playersNumber: Int, blindes: Blindes): ZIO[Console & Logger, IOException, Game] = 
    for {
      // deck <- Deck(gameConfig.cardsInDeck)
      // _ <- deck.shuffle(gameConfig.cardsInDeck)
      console <- ZIO.service[Console]
      gameStageRef <- Ref.make(0)
      currentPlayersNumberRef <- Ref.make(playersNumber)
      potRef <- Ref.make(0)
      biggestWagerRef <- Ref.make(0)
      playersConsensusNumberRef <- Ref.make(0)
      actionsRef <- Ref.make(Chunk.empty[PlayerAction])
      logger <- ZIO.service[Logger]
      
    } yield Game(
      tid,
      gid,
      gameConfig,
      blindes,
      gameStageRef,
      playersNumber,
      currentPlayersNumberRef,
      potRef,
      biggestWagerRef,
      playersConsensusNumberRef,
      actionsRef,
      logger,
      console
    )  

  

}
