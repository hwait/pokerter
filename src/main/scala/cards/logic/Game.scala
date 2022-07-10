package cards.logic
import zio._
// import GameConfig._
import java.io.IOException
import cards.domain._
import cards.services.Logger

/**
 * Represents store for Game data
 *
 * @param tid
 *   table ID
 * @param gid
 *   game ID
 * @param gameConfig
 *   game configuration from enum
 * @param blindes
 *   info about blindes and ante
 * @param gameStageRef
 *   current game stage
 * @param boardRef
 *   board cards
 * @param playersNumber
 *   max players on the table
 * @param currentPlayersNumberRef
 *   current players number on the table
 * @param potRef
 *   pot size
 * @param biggestWagerRef
 *   current biggers wager
 * @param playersConsensusNumberRef
 *   number of players agreed to start the next round
 * @param actionsRef
 *   current players actions on the table, used for decision makeng
 * @param logger
 *   the [[cards.service.Logger]] service
 * @param console
 *   the Console service
 */
case class Game private (
  tid: TableId,
  gid: GameId,
  gameConfig: GameConfigs,
  blindes: Blindes,
  gameStageRef: Ref[Int],
  boardRef: Ref[Chunk[Card]],
  playersNumber: Int,
  currentPlayersNumberRef: Ref[Int],
  potRef: Ref[Int],
  biggestWagerRef: Ref[Int],
  playersConsensusNumberRef: Ref[Int],
  actionsRef: Ref[Chunk[PlayerAction]],
  logger: Logger,
  console: Console
) {

  /** Adds card to the Board and logs it to the Logger */
  def addCard(card: Task[Card]): ZIO[Logger, Throwable, Unit] =
    for {
      logger <- ZIO.service[Logger]
      c      <- card
      _      <- boardRef.update(_ :+ c)
      stage  <- gameStageRef.get
      board  <- boardRef.get
      _      <- ZIO.when(board.size == 3)(logger.dealerMoved(DealerActions.DealsFlop, board))
      _ <-
        ZIO.when(GameStages.fromOrdinal(stage) == GameStages.Turn)(logger.dealerMoved(DealerActions.DealsTurn, board))
      _ <-
        ZIO.when(GameStages.fromOrdinal(stage) == GameStages.River)(logger.dealerMoved(DealerActions.DealsRiver, board))
    } yield ()

  /** Processes player's move and logs it to the Logger
    *
    * 1) add current action in actionsRef
    * 
    * 2) increase pot on wager size
    * 
    * 3) get player's action from Player.move 
    * 
    * 4) if player refuses to playe current game it
    *     - decrement currentPlayersNumber by one
    *     - filters out all refusant moves (actions) because they have not affect decisions
    * 
    * 5) if player moves aggressively via Bet or Rise, every rest player should make its move, so
    *     - playersConsensusNumber sets to zero
    *     - biggestWager increases by wager
    * 
    * 6) if player moves passively via Call or Check
    *     - playersConsensusNumber increases by one
    * 
    */
  def playerMove(player: Player): ZIO[Logger & Clock, Throwable, Unit] =
    for {
      logger       <- ZIO.service[Logger]
      gameStage    <- gameStageRef.get
      biggestWager <- biggestWagerRef.get
      actions      <- actionsRef.get
      board        <- boardRef.get
      pa           <- player.move(gameStage, actions, biggestWager, blindes, board)
      _            <- actionsRef.update(_ :+ pa)
      _            <- potRef.update(_ + pa.wager)
      _            <- logger.playerMoved(pa)
      _ <- if (pa.action == PlayerActions.Fold || pa.action == PlayerActions.FoldDisconnected)
             currentPlayersNumberRef.update(_ - 1) *> actionsRef.update(_.filterNot(a => a.pid == pa.pid))
           else if (
             pa.action.aggressive
           ) // If there is an aggressive move other players should make their moves. Also update the biggest wager
             playersConsensusNumberRef.update(_ => 0) *> biggestWagerRef.set(pa.wager)
           else
             playersConsensusNumberRef.update(_ + 1) // otherwise move step toward next round
    } yield ()

  /** Check players consensus for the next round */
  val isNextRound: UIO[Boolean] =
    for {
      currentPlayersN  <- currentPlayersNumberRef.get
      playersConsensus <- playersConsensusNumberRef.get
    } yield playersConsensus < currentPlayersN

  /** Renews state for the next round */
  def nextRound(): ZIO[Console, Throwable, Unit] =
    for {
      // _ <- status
      _ <- gameStageRef.update(_ + 1)
      _ <- biggestWagerRef.set(0)
      _ <- actionsRef.set(Chunk.empty[PlayerAction])
      _ <- playersConsensusNumberRef.set(0)
    } yield ()

  /** Writes current state to the console */
  val status: ZIO[Console, Throwable, Unit] =
    for {
      gameStage              <- gameStageRef.get
      currentPlayersNumber   <- currentPlayersNumberRef.get
      pot                    <- potRef.get
      biggestWager           <- biggestWagerRef.get
      playersConsensusNumber <- playersConsensusNumberRef.get
      actions                <- actionsRef.get
      _                      <- ZIO.foreach(actions)(a => console.printLine(a))
      board                  <- boardRef.get
      _ <-
        console.printLine(
          s"[T$tid / G$gid] Board:  ${board.mkString("[", ",", "]")} Pot ${pot} biggestWager ${biggestWager}, consensus ${playersConsensusNumber}/${currentPlayersNumber}."
        )
    } yield ()

  /** Calculates player result by current pot and number of winners and logs it */
  def setPlayerResult(p: Player, result: GameResults, divider: Int): ZIO[Logger, Throwable, Unit] =
    for {
      logger   <- ZIO.service[Logger]
      invested <- p.investedRef.get
      pot      <- potRef.get
      comb     <- p.combRef.get
      hc       <- p.holeCardsRef.get
      bank     <- p.bankRef.get
      profit =
        if (result == GameResults.Lost) invested * -1
        else pot / divider
      _ <- logger.playerEvaluated(p.pid, p.name, hc, result, comb, profit, bank)
    } yield ()

}
/** Factory for [[cards.logic.Game]] instances. */
object Game {
  def apply(
    tid: TableId,
    gid: GameId,
    gameConfig: GameConfigs,
    playersNumber: Int,
    blindes: Blindes
  ): ZIO[Console & Logger, IOException, Game] =
    for {
      console                   <- ZIO.service[Console]
      gameStageRef              <- Ref.make(0)
      boardRef                  <- Ref.make(Chunk.empty[Card])
      currentPlayersNumberRef   <- Ref.make(playersNumber)
      potRef                    <- Ref.make(0)
      biggestWagerRef           <- Ref.make(0)
      playersConsensusNumberRef <- Ref.make(0)
      actionsRef                <- Ref.make(Chunk.empty[PlayerAction])
      logger                    <- ZIO.service[Logger]

    } yield Game(
      tid,
      gid,
      gameConfig,
      blindes,
      gameStageRef,
      boardRef,
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
