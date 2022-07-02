package cards.domain

import zio._
import java.util.UUID

trait Logger {
  def playerMoved(pid: PlayerId, name: String, position: Int, stage: GameStages, action: PlayerActions, wager: Int = 0): Task[Unit]
  def dealerMoved(action: DealerActions, cards: Chunk[Card]): Task[Unit]
  def playerEvaluated(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int): Task[Unit]
  def saveAndClear(): Task[Unit]
  // TODO: Calc current pot from the logs, for check the Dealer's pot sustainability
}

object Logger {
  // implementation
  case class LoggerImpl(clock: Clock, console: Console, actionsRef: Ref[Chunk[GameActions]]) extends Logger {
    override def playerMoved(pid: PlayerId, name: String, position: Int, stage: GameStages, action: PlayerActions,  wager: Int): Task[Unit] =
      for {
        currentTime <- clock.nanoTime
        //pid: PlayerId, name: String, position: Int, gameStage: GameStages, action: PlayerActions, wager: Int, dt: Long)
          _ <- actionsRef.update(_ :+ PlayerAction(pid, name, position, stage,  action,  wager, currentTime))
      } yield ()

    override def dealerMoved(action: DealerActions, cards: Chunk[Card]): Task[Unit] =
      for {
        currentTime <- clock.nanoTime
          _ <- actionsRef.update(_ :+ DealerAction(action, cards, currentTime))
      } yield ()

    override def playerEvaluated(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int): Task[Unit] =
      for {
        currentTime <- clock.nanoTime
          _ <- actionsRef.update(_ :+ PlayerEvaluation(pid, result, topCombination, cards, profit, currentTime))
      } yield ()

    override def saveAndClear(): Task[Unit] = 
      for {
        /*
          Just print to console instead of save in database
        */
        actions <- actionsRef.get
        _ <- ZIO.foreach(actions)(a => console.printLine(a))
      } yield ()
  }

  // layer
  val live: ZLayer[Clock & Console, Nothing, Logger] = ZLayer {
    for {
      actionsRef <- Ref.make(Chunk.empty[GameActions])
      clock <- ZIO.service[Clock]
      console <- ZIO.service[Console]
    } yield LoggerImpl(clock, console, actionsRef)
  }

  // accessors
  def playerMoved(pid: PlayerId, name: String, position: Int, stage: GameStages, action: PlayerActions, wager: Int = 0): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.playerMoved(pid, name, position, stage, action,  wager))
    
  def dealerMoved(action: DealerActions, cards: Chunk[Card]): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.dealerMoved(action, cards))

  def playerEvaluated(pid: PlayerId, result: GameResults, topCombination: PokerCombinations, cards: Chunk[Card], profit: Int): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.playerEvaluated(pid, result, topCombination, cards, profit))
    
  def saveAndClear(): ZIO[Logger, Throwable, Unit] =
    ZIO.serviceWithZIO(_.saveAndClear())
}
