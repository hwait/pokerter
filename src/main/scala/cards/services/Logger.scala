package cards.services

import zio._
import java.util.UUID
import cards.domain._

/**
 * Logger service
 *
 * @param tableId
 *   table ID
 * @param gameConfig
 *   game config from enum [[cards.domain.GameConfigs]]
 * @param gameRef
 *   current game state [[cards.logic.Game]]
 * @param playersN
 *   TODO: does invested clears every round? players number on the table
 * @param playersRef
 *   players info [[cards.logic.Player]]
 * @param deck
 *   cards deck service [[cards.logic.Deck]]
 * @param console
 *   the Console service
 */

trait Logger {

  /**
   * Player action logging
   *
   * @param action Player Action [[cards.domain.PlayerAction]]
   */
  def playerMoved(action: PlayerAction): UIO[Unit]
  
  /**
   * Dealer action logging
   *
   * @param action  Player Action [[cards.domain.DealerActions]]
   * @param cards current board cards
   */
  def dealerMoved(action: DealerActions, cards: Chunk[Card]): UIO[Unit]
  
  /**
   * Player evaluation logging
   *
   * @param pid Player ID
   * @param name player name
   * @param holeCards player hole cards 
   * @param result game result [[cards.domain.GameResults]]
   * @param comb top player's combination [[cards.domain.BestPlayerCombination]]
   * @param profit game result in cents
   * @param bank player bank after the evaluation, in cents
   */
  def playerEvaluated(
    pid: PlayerId,
    name: String,
    holeCards: Chunk[Card],
    result: GameResults,
    comb: BestPlayerCombination,
    profit: Int,
    bank: Int
  ): UIO[Unit]

  /**
   * Flush current log to destination and clear state
   * Currently writes to console
   */
  def saveAndClear(): Task[Unit]
  // TODO: Calc current pot from the logs, for check the Dealer's pot sustainability
}

object Logger {
  /**
   * Implementation of the Logger service
   *
   * @param clock
   *   the Console service
   * @param console
   *   the Console service
   * @param actionsRef
   *   list of game actions typed by trait GameActions for combine different game actions 
   */
  case class LoggerImpl(clock: Clock, console: Console, actionsRef: Ref[Chunk[GameActions]]) extends Logger {

    override def playerMoved(action: PlayerAction): UIO[Unit] =
      for {
        _ <- actionsRef.update(_ :+ action)
      } yield ()

    override def dealerMoved(action: DealerActions, cards: Chunk[Card]): UIO[Unit] =
      for {
        currentTime <- clock.nanoTime
        _           <- actionsRef.update(_ :+ DealerAction(action, cards, currentTime))
      } yield ()

    override def playerEvaluated(
      pid: PlayerId,
      name: String,
      holeCards: Chunk[Card],
      result: GameResults,
      comb: BestPlayerCombination,
      profit: Int,
      bank: Int
    ): UIO[Unit] =
      for {
        currentTime <- clock.nanoTime
        _           <- actionsRef.update(_ :+ PlayerEvaluation(pid, name, holeCards, result, comb, profit, bank, currentTime))
      } yield ()

    override def saveAndClear(): Task[Unit] =
      for {
        /*
          Just print to console instead of save in database
         */
        actions <- actionsRef.get
        _       <- ZIO.foreach(actions)(a => console.printLine(a))
      } yield ()
  }

  // layer
  val live: ZLayer[Clock & Console, Nothing, Logger] = ZLayer {
    for {
      actionsRef <- Ref.make(Chunk.empty[GameActions])
      clock      <- ZIO.service[Clock]
      console    <- ZIO.service[Console]
    } yield LoggerImpl(clock, console, actionsRef)
  }

  // accessors
  def playerMoved(action: PlayerAction): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.playerMoved(action))

  def dealerMoved(action: DealerActions, cards: Chunk[Card]): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.dealerMoved(action, cards))

  def playerEvaluated(
    pid: PlayerId,
    name: String,
    holeCards: Chunk[Card],
    result: GameResults,
    comb: BestPlayerCombination, 
    profit: Int,
    bank: Int
  ): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.playerEvaluated(pid, name, holeCards, result, comb, profit, bank))

  def saveAndClear(): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.saveAndClear())
}
